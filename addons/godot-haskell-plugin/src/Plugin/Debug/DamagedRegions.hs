{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Plugin.Debug.DamagedRegions where

import Control.Applicative ((<|>))
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.STM
import Data.Colour
import Data.Colour.SRGB.Linear
import Data.Coerce
import qualified Data.List as List
import Data.Maybe
import qualified Data.Maybe as Maybe
import Data.Text (pack)
import Foreign.C
import Foreign.Ptr (nullPtr)
import Godot.Api.Auto (GodotDynamicFont, GodotFont, GodotImage(..), GodotImageTexture(..), GodotTexture, GodotViewport, GodotViewportTexture, Godot_ClassDB(..))
import qualified Godot.Core.GodotImage as G
import qualified Godot.Core.GodotVisualServer as VisualServer
import qualified Godot.Core.GodotViewport as G
import qualified Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import qualified Godot.Methods as G
import Godot.Nativescript
import Linear

import Plugin.Debug.DamagedRegionTypes
import Plugin.Imports
import Plugin.Types

-- Records a new damage event by (i) refreshing on-GSVS overlays and (ii) storing a HUD snapshot metadata (without the texture, since it doesn't exist yet)
recordDebugDamagedRegionEventForOverlayAndPendingHudSnapshot :: GodotSimulaViewSprite -> [GodotRect2] -> Maybe GodotTexture -> IO ()
recordDebugDamagedRegionEventForOverlayAndPendingHudSnapshot gsvs regions _maybeViewportTexture =
  when debugDamagedRegionsEnabled $ do
    -- Convert Godot Rect2s into simple (x, y, width, height) tuples
    rects <- fmap catMaybes $
      forM regions $ \region -> do
        V2 (V2 x y) (V2 width height) <- fromLowLevel region :: IO (V2 (V2 Float))
        return $
          if width > 0 && height > 0
            then Just (x, y, width, height)
            else Nothing

    -- Remove duplicates
    let uniqueRects = List.nub rects

    -- If there's real damage to process
    unless (List.null uniqueRects) $ do
      -- Read the current GSVS frame
      frame <- readTVarIO (_gsvsFrameCount gsvs)
      damageEventWasNotRecentlyShown <- atomically $ do
        -- Store the pending snapshot, checking whether this exact damage was already shown recently
        damageEventWasNotRecentlyShown <- storeDebugDamagedRegionPendingSnapshotMetadata gsvs frame uniqueRects
        -- If this event is not a recent duplicate, refresh the on-GSVS purple overlays.
        when damageEventWasNotRecentlyShown $
          modifyTVar' (_gsvsDebugDamagedRegionOverlays gsvs) $ \overlays ->
            refreshDebugDamagedRegionOverlays frame overlays uniqueRects
        return damageEventWasNotRecentlyShown
      -- Also log that a new HUD damage event happened
      when damageEventWasNotRecentlyShown $
        putStrLn $ "SIMULA_DEBUG_DAMAGED_REGIONS HUD new damage event at frame " ++ show frame

-- Only keep overlays which aren't too old
pruneOldDebugDamagedRegionOverlays :: Integer -> [DebugDamagedRegionOverlay] -> [DebugDamagedRegionOverlay]
pruneOldDebugDamagedRegionOverlays frame =
  List.filter $ \overlay ->
    frame - ddroFrame overlay <= debugDamagedRegionOverlayFrames

-- Removes expired overlays and refreshes repeated damaged regions with the current frame.
refreshDebugDamagedRegionOverlays :: Integer -> [DebugDamagedRegionOverlay] -> [DebugDamagedRegionRect] -> [DebugDamagedRegionOverlay]
refreshDebugDamagedRegionOverlays frame overlays incomingRects =
  let incomingUniqueRects = List.nub incomingRects

      -- Remove any incoming rects from older overlays, since the new overlay
      -- below will own those rects with a fresh frame timestamp.
      removeIncoming overlay =
        let remainingRects = List.filter (`notElem` incomingUniqueRects) (ddroRects overlay)
        in if List.null remainingRects
             then Nothing
             else Just overlay { ddroRects = remainingRects }

      -- Keep only non-expired old overlays, minus rects refreshed this frame.
      retainedOverlays = Maybe.mapMaybe removeIncoming (pruneOldDebugDamagedRegionOverlays frame overlays)
  in retainedOverlays ++ [DebugDamagedRegionOverlay frame incomingUniqueRects]

keepLast :: Int -> [a] -> [a]
keepLast maxItems xs =
  List.drop (List.length xs - maxItems) xs

debugDamagedRegionHudRowsGivenSnapshotCount :: Int -> Int
debugDamagedRegionHudRowsGivenSnapshotCount snapshotCount =
  max 1
    ((snapshotCount + debugDamagedRegionThumbnailsPerRow - 1) `div` debugDamagedRegionThumbnailsPerRow)

debugDamagedRegionHudHeightForRows :: Int -> Int
debugDamagedRegionHudHeightForRows rowCount =
  debugDamagedRegionThumbnailRowHeight * rowCount
    + debugDamagedRegionThumbnailGridGap * (rowCount - 1)

debugDamagedRegionHudHeightForCount :: Int -> Int
debugDamagedRegionHudHeightForCount snapshotCount =
  debugDamagedRegionHudHeightForRows (debugDamagedRegionHudRowsGivenSnapshotCount snapshotCount)

-- Height of the damaged-region thumbnail HUD region *as a whole* (including all rows)
debugDamagedRegionThumbnailHeight :: Int
debugDamagedRegionThumbnailHeight =
  debugDamagedRegionHudHeightForCount debugDamagedRegionHistoryMax

-- Stores pending HUD snapshot metadata, returning True when this is not a recent duplicate.
storeDebugDamagedRegionPendingSnapshotMetadata :: GodotSimulaViewSprite -> Integer -> [DebugDamagedRegionRect] -> STM Bool
storeDebugDamagedRegionPendingSnapshotMetadata gsvs frame uniqueRects = do
  history <- readTVar (_gsvsDebugDamagedRegionHistory gsvs)
  pendingSnapshot <- readTVar (_gsvsDebugDamagedRegionPendingSnapshot gsvs)
  let snapshotAlreadyShown =
        case pendingSnapshot <|> listToMaybe (List.reverse history) of
          Just previousSnapshot ->
            ddrsRects previousSnapshot == uniqueRects
              && frame - ddrsFrame previousSnapshot <= debugDamagedRegionDuplicateThumbnailSuppressionFrames
          Nothing -> False
  -- Assign a monotonically increasing event number for the HUD label.
  let nextEventIndex =
        case pendingSnapshot <|> listToMaybe (List.reverse history) of
          Just previousSnapshot -> ddrsEventIndex previousSnapshot + 1
          Nothing -> 1
  --       create a pending HUD snapshot for later texture capture
  unless snapshotAlreadyShown $
    writeTVar (_gsvsDebugDamagedRegionPendingSnapshot gsvs) $
      Just (DebugDamagedRegionSnapshot nextEventIndex frame uniqueRects Nothing)
  return (not snapshotAlreadyShown)

-- Finalizes a pending damaged-region HUD event by capturing the current viewport
-- texture and moving the event from pending metadata into persistent HUD history.
flushDebugDamagedRegionPendingSnapshot :: GodotSimulaViewSprite -> GodotTexture -> IO ()
flushDebugDamagedRegionPendingSnapshot gsvs viewportTexture =
  -- 1. Runs only when debugDamagedRegionsEnabled is true.
  when debugDamagedRegionsEnabled $ do
    -- 2. Reads the current GSVS frame.
    frame <- readTVarIO (_gsvsFrameCount gsvs)
    -- 3. Reads _gsvsDebugDamagedRegionPendingSnapshot, which may contain
    -- damage rect metadata from a recently observed damage event.
    pendingSnapshot <- readTVarIO (_gsvsDebugDamagedRegionPendingSnapshot gsvs)
    -- 4. If there is no pending snapshot, it does nothing.
    forM_ pendingSnapshot $ \snapshot -> do
      if frame <= ddrsFrame snapshot
        -- 5. If the pending snapshot was recorded on the current frame, it
        -- defers capture until a later invocation/frame (because this function
        -- will be called again on the next frame).
        then return ()
        else do
          -- 6. On a later frame, it calls snapshotViewportTextureForDamageHud
          -- viewportTexture.
          maybeSnapshotTexture <- snapshotViewportTextureForDamageHud viewportTexture
          atomically $ do
            currentPendingSnapshot <- readTVar (_gsvsDebugDamagedRegionPendingSnapshot gsvs)
            when (fmap ddrsEventIndex currentPendingSnapshot == Just (ddrsEventIndex snapshot)) $
              case maybeSnapshotTexture of
                Just snapshotTexture -> do
                  -- 7. If texture capture succeeds, it appends the pending
                  -- snapshot to _gsvsDebugDamagedRegionHistory with ddrsTexture
                  -- = Just snapshotTexture.
                  history <- readTVar (_gsvsDebugDamagedRegionHistory gsvs)
                  -- 8. It caps history with keepLast
                  -- debugDamagedRegionHistoryMax.
                  writeTVar (_gsvsDebugDamagedRegionHistory gsvs) $
                    keepLast debugDamagedRegionHistoryMax (history ++ [snapshot { ddrsTexture = Just snapshotTexture }])
                  -- 9. It clears the pending snapshot.
                  writeTVar (_gsvsDebugDamagedRegionPendingSnapshot gsvs) Nothing
                Nothing ->
                  -- 10. If capture fails, it leaves the pending snapshot around
                  -- for a while, then eventually clears it once it is older than
                  -- debugDamagedRegionOverlayFrames.
                  when (frame - ddrsFrame snapshot > debugDamagedRegionOverlayFrames) $
                    writeTVar (_gsvsDebugDamagedRegionPendingSnapshot gsvs) Nothing

-- Captures the current CanvasBase viewport texture (as its input argument) into
-- a new, independent ImageTexture for damaged-region HUD history. This snapshots
-- the whole viewport; damaged rects are stored separately and used later when
-- drawing the HUD preview.
snapshotViewportTextureForDamageHud :: GodotTexture -> IO (Maybe GodotTexture)
snapshotViewportTextureForDamageHud viewportTexture = do
  -- 1. Validate the incoming GodotTexture.
  case validateObject viewportTexture of
    Nothing -> return Nothing
    Just validViewportTexture -> do
      -- 2. Call G.get_data to read its pixels into a GodotImage.
      viewportImage <- G.get_data validViewportTexture
      imageIsEmpty <- G.is_empty viewportImage
      if imageIsEmpty
        then return Nothing
        else do
          -- 3. If that image is non-empty, allocate a fresh Godot Image and ImageTexture.
          classDB <- Godot_ClassDB <$> withCString "ClassDB" Api.godot_global_get_singleton
          imageVariant <- (G.instance' classDB =<< toLowLevel "Image") >>= fromLowLevel
          imageTextureVariant <- (G.instance' classDB =<< toLowLevel "ImageTexture") >>= fromLowLevel
          case (fromVariant imageVariant :: Maybe GodotObject, fromVariant imageTextureVariant :: Maybe GodotObject) of
            (Just imageObject, Just imageTextureObject) -> do
              let snapshotImage = GodotImage imageObject
              -- 4. Copy the viewport image into the fresh image.
              G.copy_from snapshotImage viewportImage
              snapshotImageIsEmpty <- G.is_empty snapshotImage
              if snapshotImageIsEmpty
                then return Nothing
                else do
                  let imageTexture = GodotImageTexture imageTextureObject
                  -- 5. Create a new ImageTexture from that copied image.
                  G.create_from_image imageTexture snapshotImage VisualServer.TEXTURE_FLAGS_DEFAULT
                  let snapshotTexture = safeCast imageTexture :: GodotTexture
                  textureWidth <- G.get_width snapshotTexture
                  textureHeight <- G.get_height snapshotTexture
                  -- 6. Return Just snapshotTexture if it has nonzero dimensions, otherwise Nothing.
                  return $
                    if textureWidth > 0 && textureHeight > 0
                      then Just snapshotTexture
                      else Nothing
            _ -> return Nothing

-- Returns the currently visible damaged-region overlays for a GSVS, pruning
-- expired overlays from GSVS state before handing them to the drawing code.
getAndPruneDebugDamagedRegionOverlays :: GodotSimulaViewSprite -> IO [DebugDamagedRegionOverlay]
getAndPruneDebugDamagedRegionOverlays gsvs =
  -- 1. If debug damaged regions are disabled, return no overlays.
  if debugDamagedRegionsEnabled
    then do
      -- 2. Read the current GSVS frame.
      frame <- readTVarIO (_gsvsFrameCount gsvs)
      atomically $ do
        -- 3. Read the overlay list from GSVS state.
        overlays <- readTVar (_gsvsDebugDamagedRegionOverlays gsvs)
        -- 4. Prune expired overlays using the current frame.
        let visible = pruneOldDebugDamagedRegionOverlays frame overlays
        -- 5. Write the pruned overlay list back into GSVS state.
        writeTVar (_gsvsDebugDamagedRegionOverlays gsvs) visible
        -- 6. Return only visible overlays to the drawing code.
        return visible
    else return []

getDebugDamagedRegionHistory :: GodotSimulaViewSprite -> IO [DebugDamagedRegionSnapshot]
getDebugDamagedRegionHistory gsvs =
  if debugDamagedRegionsEnabled
    then readTVarIO (_gsvsDebugDamagedRegionHistory gsvs)
    else return []

rememberDebugDamageRegionsFromCanvas :: CanvasSurface -> GodotSimulaViewSprite -> Maybe [GodotRect2] -> IO ()
rememberDebugDamageRegionsFromCanvas _ _ Nothing =
  return ()
rememberDebugDamageRegionsFromCanvas cs gsvs (Just regions) =
  when debugDamagedRegionsEnabled $ do
    viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
    withGodotRef (G.get_texture viewportSurface :: IO GodotViewportTexture) $ \viewportSurfaceTexture ->
      recordDebugDamagedRegionEventForOverlayAndPendingHudSnapshot gsvs regions (Just (safeCast viewportSurfaceTexture))

drawDebugHudDamagedRegionThumbnails :: CanvasBase -> [DebugDamagedRegionSnapshot] -> GodotDynamicFont -> DebugHudDamagedRegionThumbnailsArea -> IO ()
drawDebugHudDamagedRegionThumbnails cb visibleHistory debugFont debugHudDamagedRegionThumbnailsArea = do
  debugPutStrLn "Plugin.Debug.DamagedRegions.drawDebugHudDamagedRegionThumbnails"
  unless (List.null visibleHistory) $ do
    -- `visibleHistory` has already been trimmed to the recent damage
    -- snapshots that should be visible in the HUD. This function only lays
    -- out and draws those HUD cells.
    G.set_size debugFont 24
    fontHeight <- G.get_height (safeCast debugFont :: GodotFont)
    fontAscent <- G.get_ascent (safeCast debugFont :: GodotFont)
    let gap = fromIntegral debugDamagedRegionThumbnailGridGap
    let labelHeight = fontHeight + 8
    let left = debugHudDamagedRegionThumbnailsAreaLeft debugHudDamagedRegionThumbnailsArea
    let top = debugHudDamagedRegionThumbnailsAreaTop debugHudDamagedRegionThumbnailsArea
    let availableWidth = debugHudDamagedRegionThumbnailsAreaWidth debugHudDamagedRegionThumbnailsArea
    let availableHeight = debugHudDamagedRegionThumbnailsAreaHeight debugHudDamagedRegionThumbnailsArea
    let snapshotCount = List.length visibleHistory
    let columnCount = max 1 (min debugDamagedRegionThumbnailsPerRow snapshotCount)
    let rowCount = debugDamagedRegionHudRowsGivenSnapshotCount snapshotCount
    let cellWidth = max 4 ((availableWidth - gap * fromIntegral (columnCount - 1)) / fromIntegral columnCount)
    let cellHeight = max 4 ((availableHeight - gap * fromIntegral (rowCount - 1)) / fromIntegral rowCount)
    let thumbnailAreaHeight = max 4 (cellHeight - labelHeight)
    -- Likely (1, 1, 1, 1), so drawing snapshot pixels does not change the texture.
    snapshotTextureModulateColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor

    -- Iterate through the visible recent damage events. Each event gets one
    -- HUD cell in the thumbnail grid.
    forM_ (zip [0 :: Int ..] visibleHistory) $ \(snapshotIndex, snapshot) -> do

      -- Only snapshots with a captured surface texture can be drawn.
      forM_ (ddrsTexture snapshot) $ \snapshotTexture -> do
        let columnIndex = snapshotIndex `mod` columnCount
        let rowIndex = snapshotIndex `div` columnCount
        let cellLeft = left + fromIntegral columnIndex * (cellWidth + gap)
        let cellTop = top + fromIntegral rowIndex * (cellHeight + gap)
        surfaceWidth <- fromIntegral <$> G.get_width snapshotTexture
        surfaceHeight <- fromIntegral <$> G.get_height snapshotTexture
        let rects = ddrsRects snapshot

        -- Compute layout for the HUD cell:
        -- the upper abstract surface-sized damage map,
        -- the lower cropped preview,
        -- and the label area.
        let aspect = if surfaceHeight <= 0 then 1 else surfaceWidth / surfaceHeight
        let maxBoxWidth = max 4 (cellWidth - 8)
        let previewGap = 10
        let abstractAreaHeight = max 4 ((thumbnailAreaHeight - previewGap) * 0.55)
        let previewAreaHeight = max 4 (thumbnailAreaHeight - abstractAreaHeight - previewGap)
        let maxBoxHeight = max 4 (abstractAreaHeight - 8)
        let boxWidth = max 3 (min maxBoxWidth (maxBoxHeight * aspect))
        let boxHeight = max 3 (min maxBoxHeight (boxWidth / max 0.1 aspect))
        let x = cellLeft + max 0 ((cellWidth - boxWidth) / 2)
        let y = cellTop + max 0 ((abstractAreaHeight - boxHeight) / 2)
        let labelTop = cellTop + thumbnailAreaHeight
        backgroundColor <- (toLowLevel $ (rgb 0.12 0.02 0.20) `withOpacity` 0.95) :: IO GodotColor -- dark purple
        borderColor <- (toLowLevel $ (rgb 0.84 0.28 1.0) `withOpacity` 0.95) :: IO GodotColor -- bright purple
        labelColor <- (toLowLevel $ (rgb 1.0 0.86 1.0) `withOpacity` 1.0) :: IO GodotColor -- pale pink-purple
        thumbnailRect <- toLowLevel $ V2 (V2 x y) (V2 boxWidth boxHeight)

        -- Draw the abstract surface-sized damage map background. This dark
        -- purple rectangle represents the full surface, scaled down into
        -- the upper part of the HUD cell.
        G.draw_rect cb thumbnailRect backgroundColor True 1.0 False

        -- Draw literal texture pixels for each damaged piece. Each damage
        -- rect is scaled/clipped into the abstract mini-surface, then
        -- drawDebugDamagedRegionThumbnailRect copies that region from the
        -- saved snapshot texture and draws its per-rect border.
        forM_ rects $
          drawDebugDamagedRegionThumbnailRect cb snapshotTexture snapshotTextureModulateColor x y boxWidth boxHeight surfaceWidth surfaceHeight

        -- Draw the border around the whole abstract mini-surface.
        G.draw_rect cb thumbnailRect borderColor False 1.0 False

        -- Draw the cropped literal preview underneath. This computes the
        -- union/bounding box of all damaged rects and draws that source
        -- region from the snapshot texture into the lower preview area.
        drawDebugDamagedRegionPreview cb snapshotTexture snapshotTextureModulateColor borderColor (cellLeft + 4) (cellTop + abstractAreaHeight + previewGap) (cellWidth - 8) previewAreaHeight surfaceWidth surfaceHeight rects

        -- Draw the event-number label centered below this HUD cell.
        let label = show (ddrsEventIndex snapshot)
        bracket
          (toLowLevel (pack label) :: IO GodotString)
          Api.godot_string_destroy
          (\labelStr -> do
            V2 textWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) labelStr >>= fromLowLevel :: IO (V2 Float)
            labelPos <- toLowLevel (V2 (cellLeft + max 0 ((cellWidth - textWidth) / 2)) (labelTop + fontAscent)) :: IO GodotVector2
            G.draw_string cb (safeCast debugFont :: GodotFont) labelPos labelStr labelColor (round cellWidth))

-- Draws purple damaged-region rects in CanvasBase coordinates
drawDebugDamagedRegionOverlays :: CanvasBase -> GodotSimulaViewSprite -> IO ()
drawDebugDamagedRegionOverlays cb gsvs = do
  debugPutStrLn "Plugin.Debug.DamagedRegions.drawDebugDamagedRegionOverlays"
  overlays <- getAndPruneDebugDamagedRegionOverlays gsvs
  frame <- readTVarIO (gsvs ^. gsvsFrameCount)
  forM_ overlays $ \overlay -> do
    let age = max 0 (frame - ddroFrame overlay)
    let progress = min 1.0 (fromIntegral age / fromIntegral debugDamagedRegionOverlayFrames :: Double)
    let alpha = max 0.10 (0.40 * (1.0 - progress))
    forM_ (ddroRects overlay) $
      drawDebugDamagedRegionRect cb alpha

-- Draws a purple damaged-region rect overlay in CanvasBase coordinates
drawDebugDamagedRegionRect :: CanvasBase -> Double -> DebugDamagedRegionRect -> IO ()
drawDebugDamagedRegionRect cb alpha (x, y, width, height) =
  when (width > 0 && height > 0) $ do
    -- Use purple fill with the caller-supplied opacity, so the
    -- damaged pixels are visible without completely hiding the surface.
    fillColor <- (toLowLevel $ (rgb 0.70 0.18 1.0) `withOpacity` alpha) :: IO GodotColor
    -- The border is a lighter pink-purple and slightly more opaque than
    -- the fill, making the exact damaged-region edge easy to see.
    let borderAlpha = min 0.80 (alpha + 0.25)
    borderColor <- (toLowLevel $ (rgb 0.95 0.62 1.0) `withOpacity` borderAlpha) :: IO GodotColor
    debugRect <- toLowLevel $
      V2
        (V2 x y)
        (V2 width height)
    G.draw_rect cb debugRect fillColor True 1.0 False
    G.draw_rect cb debugRect borderColor False 2.0 False

-- Draws one damaged source rect inside the abstract HUD mini-surface by
-- scaling from full GSVS texture coordinates into thumbnail coordinates,
-- copying the matching snapshot pixels, and outlining the copied region.
drawDebugDamagedRegionThumbnailRect :: CanvasBase -> GodotTexture -> GodotColor -> Float -> Float -> Float -> Float -> Float -> Float -> DebugDamagedRegionRect -> IO ()
drawDebugDamagedRegionThumbnailRect cb snapshotTexture snapshotTextureModulateColor thumbnailX thumbnailY thumbnailWidth thumbnailHeight surfaceWidth surfaceHeight (x, y, width, height) = do
  -- Convert from full-size surface coordinates to the scaled-down HUD
  -- mini-surface coordinates.
  let scaleX = thumbnailWidth / max 1 surfaceWidth
  let scaleY = thumbnailHeight / max 1 surfaceHeight
  let scaledX = thumbnailX + x * scaleX
  let scaledY = thumbnailY + y * scaleY
  let scaledRight = scaledX + width * scaleX
  let scaledBottom = scaledY + height * scaleY

  -- Clamp the destination rectangle so drawing stays inside the HUD
  -- mini-surface even if damage metadata falls partly outside it.
  let clippedX = max thumbnailX scaledX
  let clippedY = max thumbnailY scaledY
  let clippedRight = min (thumbnailX + thumbnailWidth) scaledRight
  let clippedBottom = min (thumbnailY + thumbnailHeight) scaledBottom

  -- Clamp the source rectangle so texture sampling stays inside the saved
  -- snapshot texture.
  let sourceX = max 0 x
  let sourceY = max 0 y
  let sourceRight = min surfaceWidth (x + width)
  let sourceBottom = min surfaceHeight (y + height)

  -- Only draw if clipping left a real destination rect and a real source
  -- texture region.
  when (clippedRight > clippedX && clippedBottom > clippedY && sourceRight > sourceX && sourceBottom > sourceY) $ do
    borderColor <- (toLowLevel $ (rgb 1.0 0.66 1.0) `withOpacity` 0.92) :: IO GodotColor
    debugRect <- toLowLevel $
      V2
        (V2 clippedX clippedY)
        (V2 (clippedRight - clippedX) (clippedBottom - clippedY))

    -- Copy the actual damaged pixels into the mini-surface, then draw a
    -- thin pink-purple border around that copied piece.
    drawSnapshotTextureRegionIntoCanvasBaseRect cb snapshotTexture snapshotTextureModulateColor debugRect (sourceX, sourceY, sourceRight - sourceX, sourceBottom - sourceY)
    G.draw_rect cb debugRect borderColor False 1.0 False

-- Draws the lower cropped preview for one damage event in the HUD cell.
-- It finds the bounding box around all damaged rects, fits that cropped
-- region into the available preview area, then delegates per-rect pixel
-- copying and outlining to drawDebugDamagedRegionPreviewRects.
drawDebugDamagedRegionPreview :: CanvasBase -> GodotTexture -> GodotColor -> GodotColor -> Float -> Float -> Float -> Float -> Float -> Float -> [DebugDamagedRegionRect] -> IO ()
drawDebugDamagedRegionPreview cb snapshotTexture snapshotTextureModulateColor borderColor x y availableWidth availableHeight surfaceWidth surfaceHeight rects =
  forM_ (debugDamagedRegionUnionRectClampedToGSVSBounds surfaceWidth surfaceHeight rects) $ \unionRect@(_, _, sourceWidth, sourceHeight) -> do
    let aspect = if sourceHeight <= 0 then 1 else sourceWidth / sourceHeight
    let previewWidth = max 3 (min availableWidth (availableHeight * aspect))
    let previewHeight = max 3 (min availableHeight (previewWidth / max 0.1 aspect))
    let previewX = x + max 0 ((availableWidth - previewWidth) / 2)
    let previewY = y + max 0 ((availableHeight - previewHeight) / 2)
    drawDebugDamagedRegionPreviewRects cb snapshotTexture snapshotTextureModulateColor borderColor previewX previewY previewWidth previewHeight unionRect rects

-- Draws every damaged rect that intersects the cropped preview's union
-- rect. Each rect is clipped to that union, scaled into the preview area,
-- filled with the matching snapshot pixels, and outlined.
drawDebugDamagedRegionPreviewRects :: CanvasBase -> GodotTexture -> GodotColor -> GodotColor -> Float -> Float -> Float -> Float -> DebugDamagedRegionRect -> [DebugDamagedRegionRect] -> IO ()
drawDebugDamagedRegionPreviewRects cb snapshotTexture snapshotTextureModulateColor borderColor previewX previewY previewWidth previewHeight (unionX, unionY, unionWidth, unionHeight) rects = do
  let scaleX = previewWidth / max 1 unionWidth
  let scaleY = previewHeight / max 1 unionHeight
  forM_ rects $ \(rectX, rectY, rectWidth, rectHeight) -> do
    let sourceX = max unionX rectX
    let sourceY = max unionY rectY
    let sourceRight = min (unionX + unionWidth) (rectX + rectWidth)
    let sourceBottom = min (unionY + unionHeight) (rectY + rectHeight)
    when (sourceRight > sourceX && sourceBottom > sourceY) $ do
      let destinationX = previewX + (sourceX - unionX) * scaleX
      let destinationY = previewY + (sourceY - unionY) * scaleY
      let destinationWidth = (sourceRight - sourceX) * scaleX
      let destinationHeight = (sourceBottom - sourceY) * scaleY
      previewRect <- toLowLevel $ V2 (V2 destinationX destinationY) (V2 destinationWidth destinationHeight)
      drawSnapshotTextureRegionIntoCanvasBaseRect cb snapshotTexture snapshotTextureModulateColor previewRect (sourceX, sourceY, sourceRight - sourceX, sourceBottom - sourceY)
      G.draw_rect cb previewRect borderColor False 1.0 False

-- Draws one damaged source rectangle from the captured viewport texture into
-- the requested HUD destination rectangle.
drawSnapshotTextureRegionIntoCanvasBaseRect :: CanvasBase -> GodotTexture -> GodotColor -> GodotRect2 -> DebugDamagedRegionRect -> IO ()
drawSnapshotTextureRegionIntoCanvasBaseRect cb snapshotTexture snapshotTextureModulateColor destinationRect (x, y, width, height) =
  -- snapshotTexture will be supplied from DebugDamagedRegionSnapshot.ddrsTexture
  -- destinationRect is in CanvasBase coordinates, which includes the normal
  -- surfaces smashed together AND the bounds of the FULL HUD (including
  -- non-damaged region stuff depending upon whether it is enabled).
  -- (x, y, width, height) is in snapshotTexture/source texture coordinates.
  -- 1. Skip empty source rectangles.
  when (width > 0 && height > 0) $ do
    -- 2. Convert the damaged-region tuple into the Godot Rect2 source
    -- region to sample from snapshotTexture.
    sourceRect <- toLowLevel $ V2 (V2 x y) (V2 width height)
    -- 3. Draw only that source region from snapshotTexture into the
    -- destinationRect on the CanvasBase HUD.
    G.draw_texture_rect_region cb snapshotTexture destinationRect sourceRect snapshotTextureModulateColor False (coerce nullPtr) True

-- Computes the smallest GSVS-coordinate rectangle that contains all
-- positive-area damaged rects, clamped to the GSVS source bounds (first two arguments).
debugDamagedRegionUnionRectClampedToGSVSBounds :: Float -> Float -> [DebugDamagedRegionRect] -> Maybe DebugDamagedRegionRect
debugDamagedRegionUnionRectClampedToGSVSBounds surfaceWidth surfaceHeight rects =
  -- 1. Drop empty damaged rects before computing a bounding box.
  case List.filter positiveArea rects of
    -- 2. If no damaged rects have positive area, there is no union rect.
    [] -> Nothing
    nonEmptyRects ->
      -- 3. Compute the damaged bounding box edges in source surface
      -- coordinates, clamping them to the valid surface bounds.
      let left = maximum [0, minimum (fmap (\(x, _, _, _) -> x) nonEmptyRects)]
          top = maximum [0, minimum (fmap (\(_, y, _, _) -> y) nonEmptyRects)]
          right = minimum [surfaceWidth, maximum (fmap (\(x, _, width, _) -> x + width) nonEmptyRects)]
          bottom = minimum [surfaceHeight, maximum (fmap (\(_, y, _, height) -> y + height) nonEmptyRects)]
      -- 4. Return the clamped union rect only if it still has positive area.
      in if right > left && bottom > top
           then Just (left, top, right - left, bottom - top)
           else Nothing
  where
    -- 5. A damaged rect only contributes to the union if width and height
    -- are both positive.
    positiveArea (_, _, width, height) = width > 0 && height > 0
