{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.CanvasBase where

import Control.Exception
import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import           Godot.Nativescript
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Godot.Core.GodotViewport as G

import Godot.Core.GodotVisualServer as G

import Plugin.Types
import Data.Maybe
import Data.Either

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable

import qualified Data.Map.Strict as M
import qualified Data.List

import Data.Map.Ordered as MO

instance Eq GodotWlrSurface where
  wlrSurface1 == wlrSurface2 = ((coerce wlrSurface1) :: Ptr ()) == ((coerce wlrSurface2) :: Ptr ())

instance Ord GodotWlrSurface where
  wlrSurface1 `compare` wlrSurface2 = ((coerce wlrSurface1) :: Ptr ()) `compare` ((coerce wlrSurface2) :: Ptr ())

instance Eq CanvasBase where
  (==) = (==) `on` _cbObject

instance NativeScript CanvasBase where
  className = "CanvasBase"
  classInit obj = do
    CanvasBase (safeCast obj)
                  <$> atomically (newTVar (error "Failed to initialize CanvasBase."))
                  <*> atomically (newTVar (error "Failed to initialize CanvasBase."))
                  <*> atomically (newTVar (error "Failed to initialize CanvasBase."))
  classMethods =
    [
      func NoRPC "_process" (catchGodot Plugin.CanvasBase._process)
    , func NoRPC "_draw" (catchGodot Plugin.CanvasBase._draw)
    , func NoRPC "_ready" (catchGodot Plugin.CanvasBase._ready)
    ]

newCanvasBase :: GodotSimulaViewSprite -> IO (CanvasBase)
newCanvasBase gsvs = do
  debugPutStrLn "Plugin.CanvasBase.newCanvasBase"
  cb <- "res://addons/godot-haskell-plugin/CanvasBase.gdns"
    & newNS' []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr :: IO CanvasBase

  viewport <- initializeRenderTarget gsvs ViewportBase
  debugFont <- unsafeInstance GodotDynamicFont "DynamicFont"
  dynamicFontData' <- load GodotDynamicFontData "DynamicFontData" "res://OpenSansEmoji.ttf"
  let dynamicFontData = Data.Maybe.fromJust dynamicFontData'
  G.set_font_data debugFont dynamicFontData
  G.set_size debugFont 24

  atomically $ writeTVar (_cbGSVS cb) gsvs
  atomically $ writeTVar (_cbViewport cb) viewport
  atomically $ writeTVar (_cbDebugFont cb) debugFont

  return cb

_ready :: CanvasBase -> [GodotVariant] -> IO ()
_ready cb gvArgs = do
  debugPutStrLn "Plugin.CanvasBase._ready"
  G.set_process cb True
  mapM_ Api.godot_variant_destroy gvArgs

_process :: CanvasBase -> [GodotVariant] -> IO ()
_process self gvArgs = do
  debugPutStrLn "Plugin.CanvasBase._process"
  G.update self
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

_draw :: CanvasBase -> [GodotVariant] -> IO ()
_draw cb gvArgs = do
  debugPutStrLn "Plugin.CanvasBase._draw"
  gsvs <- readTVarIO (cb ^. cbGSVS)

  when debugSurfaceBoundariesEnabled $
    drawDebugBackground cb gsvs

  drawCanvasSurface cb gsvs

  when debugSurfaceBoundariesEnabled $ do
    -- Outline raw wlr_surface buffers as red
    drawRedWlrSurfaceBoundaries cb gsvs

    when (debugMouseEventsEnabled || debugKeyboardEventsEnabled) $
      -- Show pointer IDs inside surfaces when debugging mouse/keyboard events (where it's likely useful)
      drawRedWlrSurfacePointerIds cb gsvs

    -- Show xdg/xwayland geometry rectangles as blue WITHOUT their x/y offsets
    drawBlueGeometryBordersWithoutOffsets cb gsvs

    -- Show xdg/xwayland geometry rectangles as green WITH their x/y offsets
    drawGreenGeometryBordersWithOffsets cb gsvs

  -- Draw cursor
  drawCursor cb gsvs

  -- Increment global framecount
  atomically $ modifyTVar' (gsvs ^. gsvsFrameCount) (+1)

  mapM_ Api.godot_variant_destroy gvArgs

  where
    getTransparency :: CanvasBase -> IO Double
    getTransparency cb = do
      debugPutStrLn "Plugin.CanvasBase.getTransparency"
      gsvs <- readTVarIO (cb ^. cbGSVS)
      gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
      return (realToFrac gsvsTransparency)
    savePngCS :: (GodotWlrSurface, CanvasSurface) -> IO ()
    savePngCS arg@((wlrSurface, cs)) = do
      debugPutStrLn "Plugin.CanvasBase.savePngCS"
      viewportSurface <- readTVarIO (cs ^. csViewport) :: IO GodotViewport
      withGodotRef (G.get_texture (viewportSurface :: GodotViewport) :: IO GodotViewportTexture) $ \viewportSurfaceTexture ->
        savePng cs viewportSurfaceTexture wlrSurface >> return ()

    drawDebugBackground :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawDebugBackground cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawDebugBackground"
      cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
      viewportSurface <- readTVarIO (cs ^. csViewport)
      V2 width height <- G.get_size viewportSurface >>= fromLowLevel :: IO (V2 Float)
      debugRect <- toLowLevel $ V2 (V2 0 0) (V2 width height)

      view <- readTVarIO (gsvs ^. gsvsView)
      let eitherSurface = view ^. svWlrEitherSurface
      backgroundColor <- case eitherSurface of
        Left _  -> (toLowLevel $ (rgb 1.0 (188/255.0) 0.0) `withOpacity` 1.0) :: IO GodotColor -- Orangish yellow for XDG
        Right _ -> (toLowLevel $ (rgb 0.0 0.0 0.0) `withOpacity` 1.0) :: IO GodotColor         -- Black for XWayland

      G.draw_rect cb debugRect backgroundColor True 1.0 False

    drawCanvasSurface :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawCanvasSurface cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawCanvasSurface"
      cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
      viewportSurface <- readTVarIO (cs ^. csViewport)
      renderPosition <- toLowLevel (V2 0 0) :: IO GodotVector2
      gsvsTransparency <- getTransparency cb
      modulateColor <- (toLowLevel $ (rgb 1.0 1.0 (1.0 :: Double)) `withOpacity` gsvsTransparency) :: IO GodotColor

      withGodotRef (G.get_texture viewportSurface :: IO GodotViewportTexture) $ \viewportSurfaceTexture ->
        G.draw_texture cb ((safeCast viewportSurfaceTexture) :: GodotTexture) renderPosition modulateColor (coerce nullPtr)

    drawRedWlrSurfaceBoundaries :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawRedWlrSurfaceBoundaries cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawRedWlrSurfaceBoundaries"
      redColor <- (toLowLevel $ (rgb 1.0 0.0 0.0) `withOpacity` 1.0) :: IO GodotColor
      bracket
        (getDepthFirstSurfaces gsvs)
        destroyWlrSurfacesWithCoords
        (\depthFirstSurfaces ->
          forM_ depthFirstSurfaces $ \(wlrSurface, x, y) -> do
            (width, height) <- getBufferDimensions wlrSurface
            debugRect <- toLowLevel $
              V2
                (V2 (fromIntegral x) (fromIntegral y))
                (V2 (fromIntegral width) (fromIntegral height))
            G.draw_rect cb debugRect redColor False 2.0 False)

    drawRedWlrSurfacePointerIds :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawRedWlrSurfacePointerIds cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawRedWlrSurfacePointerIds"
      redColor <- (toLowLevel $ (rgb 1.0 0.0 0.0) `withOpacity` 1.0) :: IO GodotColor
      debugFont <- readTVarIO (cb ^. cbDebugFont)
      bracket
        (getDepthFirstSurfaces gsvs)
        destroyWlrSurfacesWithCoords
        (\depthFirstSurfaces -> do
          labelGroups <- foldM addSurfaceLabel M.empty depthFirstSurfaces
          forM_ (M.elems labelGroups) $
            drawSurfaceLabelGroup cb debugFont redColor)
      where
        addSurfaceLabel :: M.Map (Int, Int) ((Int, Int, Int, Int), [String]) -> (GodotWlrSurface, Int, Int) -> IO (M.Map (Int, Int) ((Int, Int, Int, Int), [String]))
        addSurfaceLabel labelsByCenter (wlrSurface, x, y) = do
          (width, height) <- getBufferDimensions wlrSurface
          if width > 0 && height > 0
            then do
              label <- getSurfaceLabel wlrSurface width height
              let labelGroup = ((x, y, width, height), [label])
              return $ M.insertWith appendLabelGroup (surfaceCenterKey x y width height) labelGroup labelsByCenter
            else return labelsByCenter

        appendLabelGroup :: ((Int, Int, Int, Int), [String]) -> ((Int, Int, Int, Int), [String]) -> ((Int, Int, Int, Int), [String])
        appendLabelGroup (_, newLabels) (oldRect, oldLabels) =
          (oldRect, oldLabels ++ newLabels)

        surfaceCenterKey :: Int -> Int -> Int -> Int -> (Int, Int)
        surfaceCenterKey x y width height =
          (2 * x + width, 2 * y + height)

        getSurfaceLabel :: GodotWlrSurface -> Int -> Int -> IO String
        getSurfaceLabel wlrSurface width height = do
          surfaceRole <- getWlrSurfaceRoleName wlrSurface
          return $ surfaceRole ++ ":" ++ pointerIdSuffix width height (show wlrSurface)

        getWlrSurfaceRoleName :: GodotWlrSurface -> IO String
        getWlrSurfaceRoleName wlrSurface = do
          isSubsurface <- G.is_wlr_subsurface wlrSurface
          isXdgSurface <- G.is_wlr_xdg_surface wlrSurface
          isXWaylandSurface <- G.is_wlr_xwayland_surface wlrSurface
          return $
            case (isSubsurface, isXdgSurface, isXWaylandSurface) of
              (True, _, _) -> "sub"
              (_, True, _) -> "xdg"
              (_, _, True) -> "xway"
              _ -> "wlr"

        pointerIdSuffix :: Int -> Int -> String -> String
        pointerIdSuffix width height pointerId =
          let suffixLength = pointerIdSuffixLength width height
          in case splitAt (length pointerId - suffixLength) pointerId of
            (_, suffix) | length pointerId > 10 -> suffix
            _ -> pointerId

        pointerIdSuffixLength :: Int -> Int -> Int
        pointerIdSuffixLength width height =
          if width >= 140 && height >= 24 then 7 else 4

    drawSurfaceLabelGroup :: CanvasBase -> GodotDynamicFont -> GodotColor -> ((Int, Int, Int, Int), [String]) -> IO ()
    drawSurfaceLabelGroup cb font color ((x, y, width, height), labels) = do
      debugPutStrLn "Plugin.CanvasBase.drawSurfaceLabelGroup"
      maybeFitted <- fitSurfaceLabels font labels width height
      case maybeFitted of
        Nothing -> return ()
        Just (fontSize, visibleLabels) ->
          drawCenteredLabelLines cb font color fontSize visibleLabels x y width height

    drawCenteredLabelLines :: CanvasBase -> GodotDynamicFont -> GodotColor -> Int -> [String] -> Int -> Int -> Int -> Int -> IO ()
    drawCenteredLabelLines cb font color fontSize labels x y width height = do
      debugPutStrLn "Plugin.CanvasBase.drawCenteredLabelLines"
      G.set_size font fontSize
      fontHeight <- G.get_height (safeCast font :: GodotFont)
      bracket
        (mapM (toLowLevel . pack) labels :: IO [GodotString])
        (mapM_ Api.godot_string_destroy)
        (\labelStrs -> do
          fontAscent <- G.get_ascent (safeCast font :: GodotFont)
          let surfaceWidth = fromIntegral width
          let surfaceHeight = fromIntegral height
          let labelCount = length labelStrs
          let blockHeight = fontHeight * fromIntegral labelCount
          let firstBaselineY = fromIntegral y + max fontAscent ((surfaceHeight - blockHeight) / 2 + fontAscent)
          forM_ (zip [0..] labelStrs) $ \(lineIndex :: Int, labelStr) -> do
            V2 textWidth _ <- G.get_string_size (safeCast font :: GodotFont) labelStr >>= fromLowLevel :: IO (V2 Float)
            let textX = fromIntegral x + max 0 ((surfaceWidth - textWidth) / 2)
            let baselineY = firstBaselineY + fontHeight * fromIntegral lineIndex
            renderPosition <- toLowLevel (V2 textX baselineY) :: IO GodotVector2
            G.draw_string cb (safeCast font :: GodotFont) renderPosition labelStr color width)

    fitSurfaceLabels :: GodotDynamicFont -> [String] -> Int -> Int -> IO (Maybe (Int, [String]))
    fitSurfaceLabels font labels width height =
      findFittingFontSize candidateSizes
      where
        minReadableFontSize = 14
        maxFontSize = max minReadableFontSize (min 24 (height - 4))
        candidateSizes = [maxFontSize, maxFontSize - 1 .. minReadableFontSize]

        fitLabelCount :: Int -> [String] -> [String]
        fitLabelCount maxLines labels
          | maxLines <= 0 = []
          | length labels <= maxLines = labels
          | maxLines <= 1 = take 1 labels
          | otherwise = take (maxLines - 1) labels ++ ["+" ++ show (length labels - maxLines + 1)]

        findFittingFontSize :: [Int] -> IO (Maybe (Int, [String]))
        findFittingFontSize [] = return Nothing
        findFittingFontSize (fontSize:rest) = do
          G.set_size font fontSize
          fontHeight <- G.get_height (safeCast font :: GodotFont)
          let maxLines = floor ((fromIntegral (max 1 height) - 4) / max 1 fontHeight)
          let visibleLabels = fitLabelCount maxLines labels
          if Prelude.null visibleLabels
            then return Nothing
            else do
              textWidths <- forM visibleLabels $ \label ->
                bracket
                  (toLowLevel (pack label) :: IO GodotString)
                  Api.godot_string_destroy
                  (\labelStr -> do
                    V2 textWidth _ <- G.get_string_size (safeCast font :: GodotFont) labelStr >>= fromLowLevel :: IO (V2 Float)
                    return textWidth)
              let maxWidth = max 1 (fromIntegral width - 4)
              let maxHeight = max 1 (fromIntegral height - 4)
              let totalHeight = fontHeight * fromIntegral (length visibleLabels)
              if maximum textWidths <= maxWidth && totalHeight <= maxHeight
                then return (Just (fontSize, visibleLabels))
                else findFittingFontSize rest

    drawBlueGeometryBordersWithoutOffsets :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawBlueGeometryBordersWithoutOffsets cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawBlueGeometryBordersWithoutOffsets"
      blueColor <- (toLowLevel $ (rgb 0.0 0.3 1.0) `withOpacity` 1.0) :: IO GodotColor
      simulaView <- readTVarIO (gsvs ^. gsvsView)
      geometryRects <- case (simulaView ^. svWlrEitherSurface) of
        Left wlrXdgSurface ->
          getXdgGeometryRootRects 0 0 wlrXdgSurface
        Right wlrXWaylandSurface -> do
          rootRects <- getXWaylandGeometryRootRects 0 0 wlrXWaylandSurface
          freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
          freeChildRects <- fmap Prelude.concat $
            forM freeChildren $ \freeChild -> do
              childX <- G.get_x freeChild
              childY <- G.get_y freeChild
              getXWaylandGeometryRootRects childX childY freeChild
          return (rootRects ++ freeChildRects)
      forM_ geometryRects $ \(x, y, width, height) -> do
        debugRect <- toLowLevel $
          V2
            (V2 (fromIntegral x) (fromIntegral y))
            (V2 (fromIntegral width) (fromIntegral height))
        G.draw_rect cb debugRect blueColor False 4.0 False

    drawGreenGeometryBordersWithOffsets :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawGreenGeometryBordersWithOffsets cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawGreenGeometryBordersWithOffsets"
      greenColor <- (toLowLevel $ (rgb 0.0 1.0 0.0) `withOpacity` 1.0) :: IO GodotColor
      simulaView <- readTVarIO (gsvs ^. gsvsView)
      geometryRects <- case (simulaView ^. svWlrEitherSurface) of
        Left wlrXdgSurface ->
          getXdgGeometryRects 0 0 wlrXdgSurface
        Right wlrXWaylandSurface -> do
          rootRects <- getXWaylandGeometryRects 0 0 wlrXWaylandSurface
          freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
          freeChildRects <- fmap Prelude.concat $
            forM freeChildren $ \freeChild -> do
              childX <- G.get_x freeChild
              childY <- G.get_y freeChild
              getXWaylandGeometryRects childX childY freeChild
          return (rootRects ++ freeChildRects)
      forM_ geometryRects $ \(x, y, width, height) -> do
        debugRect <- toLowLevel $
          V2
            (V2 (fromIntegral x) (fromIntegral y))
            (V2 (fromIntegral width) (fromIntegral height))
        G.draw_rect cb debugRect greenColor False 2.0 False

    getXdgGeometryRootRects :: Int -> Int -> GodotWlrXdgSurface -> IO [(Int, Int, Int, Int)]
    getXdgGeometryRootRects rootX rootY wlrXdgSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXdgGeometryRootRects"
      validateSurfaceE wlrXdgSurface
      rect <- getXdgGeometryRootRect rootX rootY wlrXdgSurface
      popupChildren <- getMappedXdgPopupChildrenAndRoots wlrXdgSurface
      childRects <- fmap Prelude.concat $
        forM popupChildren $ \(popupChild, popupRootX, popupRootY) ->
          getXdgGeometryRootRects popupRootX popupRootY popupChild
      return (rect : childRects)

    getXdgGeometryRects :: Int -> Int -> GodotWlrXdgSurface -> IO [(Int, Int, Int, Int)]
    getXdgGeometryRects rootX rootY wlrXdgSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXdgGeometryRects"
      validateSurfaceE wlrXdgSurface
      rect <- getXdgGeometryRect rootX rootY wlrXdgSurface
      popupChildren <- getMappedXdgPopupChildrenAndRoots wlrXdgSurface
      childRects <- fmap Prelude.concat $
        forM popupChildren $ \(popupChild, popupRootX, popupRootY) ->
          getXdgGeometryRects popupRootX popupRootY popupChild
      return (rect : childRects)

    getMappedXdgPopupChildrenAndRoots :: GodotWlrXdgSurface -> IO [(GodotWlrXdgSurface, Int, Int)]
    getMappedXdgPopupChildrenAndRoots wlrXdgSurface = do
      debugPutStrLn "Plugin.CanvasBase.getMappedXdgPopupChildrenAndRoots"
      arrayOfChildren <- G.get_children wlrXdgSurface :: IO GodotArray
      arrayOfChildrenGV <- fromGodotArray arrayOfChildren
      children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXdgSurface]
      validatedChildren <- mapM validateSurfaceE children
      childrenAsPopups <- mapM G.get_xdg_popup validatedChildren
      childrenX <- mapM G.get_x childrenAsPopups
      childrenY <- mapM G.get_y childrenAsPopups
      childrenGeometryOffsets <-
        mapM
          (\child -> do
            V2 (V2 childGeomX childGeomY) _ <- G.get_geometry child >>= fromLowLevel :: IO (V2 (V2 Float))
            return (round childGeomX, round childGeomY))
          validatedChildren
      Api.godot_array_destroy arrayOfChildren
      mapM_ Api.godot_variant_destroy arrayOfChildrenGV
      return $
        Data.List.zipWith4
          (\child childX childY (childGeomX, childGeomY) ->
            (child, childX - childGeomX, childY - childGeomY))
          validatedChildren
          childrenX
          childrenY
          childrenGeometryOffsets

    getXWaylandGeometryRootRects :: Int -> Int -> GodotWlrXWaylandSurface -> IO [(Int, Int, Int, Int)]
    getXWaylandGeometryRootRects rootX rootY wlrXWaylandSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXWaylandGeometryRootRects"
      validateSurfaceE wlrXWaylandSurface
      rect <- getXWaylandGeometryRootRect rootX rootY wlrXWaylandSurface
      mappedChildren <- getMappedXWaylandChildrenAndRoots wlrXWaylandSurface
      childRects <- fmap Prelude.concat $
        forM mappedChildren $ \(child, childX, childY) ->
          getXWaylandGeometryRootRects childX childY child
      return (rect : childRects)

    getXWaylandGeometryRects :: Int -> Int -> GodotWlrXWaylandSurface -> IO [(Int, Int, Int, Int)]
    getXWaylandGeometryRects rootX rootY wlrXWaylandSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXWaylandGeometryRects"
      validateSurfaceE wlrXWaylandSurface
      rect <- getXWaylandGeometryRect rootX rootY wlrXWaylandSurface
      mappedChildren <- getMappedXWaylandChildrenAndRoots wlrXWaylandSurface
      childRects <- fmap Prelude.concat $
        forM mappedChildren $ \(child, childX, childY) ->
          getXWaylandGeometryRects childX childY child
      return (rect : childRects)

    getMappedXWaylandChildrenAndRoots :: GodotWlrXWaylandSurface -> IO [(GodotWlrXWaylandSurface, Int, Int)]
    getMappedXWaylandChildrenAndRoots wlrXWaylandSurface = do
      debugPutStrLn "Plugin.CanvasBase.getMappedXWaylandChildrenAndRoots"
      arrayOfChildren <- G.get_children wlrXWaylandSurface :: IO GodotArray
      arrayOfChildrenGV <- fromGodotArray arrayOfChildren
      children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXWaylandSurface]
      validatedChildren <- mapM validateSurfaceE children
      childrenX <- mapM G.get_x validatedChildren
      childrenY <- mapM G.get_y validatedChildren
      Api.godot_array_destroy arrayOfChildren
      mapM_ Api.godot_variant_destroy arrayOfChildrenGV
      return $ zip3 validatedChildren childrenX childrenY

    getXdgGeometryRect :: Int -> Int -> GodotWlrXdgSurface -> IO (Int, Int, Int, Int)
    getXdgGeometryRect rootX rootY wlrXdgSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXdgGeometryRect"
      V2 (V2 geometryX geometryY) (V2 geometryWidth geometryHeight) <-
        G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
      return
        ( rootX + round geometryX
        , rootY + round geometryY
        , round geometryWidth
        , round geometryHeight
        )

    getXdgGeometryRootRect :: Int -> Int -> GodotWlrXdgSurface -> IO (Int, Int, Int, Int)
    getXdgGeometryRootRect rootX rootY wlrXdgSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXdgGeometryRootRect"
      V2 _ (V2 geometryWidth geometryHeight) <-
        G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
      return
        ( rootX
        , rootY
        , round geometryWidth
        , round geometryHeight
        )

    getXWaylandGeometryRect :: Int -> Int -> GodotWlrXWaylandSurface -> IO (Int, Int, Int, Int)
    getXWaylandGeometryRect rootX rootY wlrXWaylandSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXWaylandGeometryRect"
      V2 (V2 geometryX geometryY) (V2 geometryWidth geometryHeight) <-
        G.get_geometry wlrXWaylandSurface >>= fromLowLevel :: IO (V2 (V2 Float))
      return
        ( rootX + round geometryX
        , rootY + round geometryY
        , round geometryWidth
        , round geometryHeight
        )

    getXWaylandGeometryRootRect :: Int -> Int -> GodotWlrXWaylandSurface -> IO (Int, Int, Int, Int)
    getXWaylandGeometryRootRect rootX rootY wlrXWaylandSurface = do
      debugPutStrLn "Plugin.CanvasBase.getXWaylandGeometryRootRect"
      V2 _ (V2 geometryWidth geometryHeight) <-
        G.get_geometry wlrXWaylandSurface >>= fromLowLevel :: IO (V2 (V2 Float))
      return
        ( rootX
        , rootY
        , round geometryWidth
        , round geometryHeight
        )

    drawCursor :: CanvasBase -> GodotSimulaViewSprite -> IO ()
    drawCursor cb gsvs = do
      debugPutStrLn "Plugin.CanvasBase.drawCursor"
      activeGSVSCursorPos@(SurfaceLocalCoordinates (sx, sy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
      gss <- readTVarIO (gsvs ^. gsvsServer)
      (maybeWlrSurfaceCursor, maybeCursorTexture) <- readTVarIO (gsvs ^. gsvsCursor)
      maybeScreenshotCursorTexture <- readTVarIO (gss ^. gssScreenshotCursorTexture)
      screenshotModeEnabled <- readTVarIO (gsvs ^. gsvsScreenshotMode)

      -- Fork behavior depending upon whether screenshot mode is enabled
      case (screenshotModeEnabled, maybeWlrSurfaceCursor, maybeScreenshotCursorTexture, maybeCursorTexture)  of
        (False, Just wlrSurfaceCursor, _, _) -> do
           -- Draw client provided cursor
           validateSurfaceE wlrSurfaceCursor
           cursorRenderPosition <- toLowLevel (V2 sx sy) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           withGodotRef (G.get_texture wlrSurfaceCursor :: IO GodotTexture) $ \cursorTexture ->
             G.draw_texture cb cursorTexture cursorRenderPosition godotColor (coerce nullPtr)
           G.send_frame_done wlrSurfaceCursor
        (False, Nothing, _, Just cursorTexture) -> do
           -- Draw default cursor
           cursorRenderPosition <- toLowLevel (V2 sx sy) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           G.draw_texture cb cursorTexture cursorRenderPosition godotColor (coerce nullPtr)
        (True, _, Just screenshotCursorTexture, _) -> do
           -- Draw screenshot cursor
           cursorRenderPosition <- toLowLevel (V2 (sx - 16) (sy - 16)) :: IO GodotVector2
           godotColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
           G.draw_texture cb screenshotCursorTexture cursorRenderPosition godotColor (coerce nullPtr)

           screenshotCoords@(origin, end) <- readTVarIO (gsvs ^. gsvsScreenshotCoords)
           case (origin, end) of
             (Just (SurfaceLocalCoordinates (ox, oy)), Nothing) -> do
               -- Allow user to see screenshot region
               activeGSVSCursorPos@(SurfaceLocalCoordinates (cx, cy)) <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
               let sizeX = cx - ox
               let sizeY = cy - oy
               let m22Rect = V2 (V2 ox oy) (V2  sizeX sizeY)
               m22Rect' <- toLowLevel m22Rect
               grayColor <- (toLowLevel $ (rgb 0.0 0.0 0.0) `withOpacity` 0.5) :: IO GodotColor
               G.draw_rect cb m22Rect' grayColor False 2.0 False
             (Just (SurfaceLocalCoordinates (ox, oy)), Just (SurfaceLocalCoordinates (ex, ey))) -> do
               putStrLn $ "Screenshot mode: taking screenshot from (" ++ (show ox) ++ ", " ++ (show oy) ++ ") to (" ++ (show ex) ++ ", " ++ (show ey) ++ ")"

               -- Take screenshot & save to X clipboard
               let m22Rect = V2 (V2 ox oy) (V2 (ex - ox) (ey - oy))
               viewportSurface <- readTVarIO (cb ^. cbViewport) :: IO GodotViewport
               withGodotRef (G.get_texture (viewportSurface :: GodotViewport) :: IO GodotViewportTexture) $ \viewportSurfaceTexture ->
                 saveViewportAsPngAndLaunch gsvs viewportSurfaceTexture m22Rect

               -- Disable screenshot
               atomically $ writeTVar (gsvs ^. gsvsScreenshotMode) False
               atomically $ writeTVar (gsvs ^. gsvsScreenshotCoords) (Nothing, Nothing)
             _ -> return ()
        _ -> return ()
