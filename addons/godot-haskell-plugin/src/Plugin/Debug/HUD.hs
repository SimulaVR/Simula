{-# LANGUAGE TypeApplications #-}

module Plugin.Debug.HUD where

import Control.Exception
import Control.Lens hiding (Context)
import Control.Monad
import Control.Concurrent.STM.TVar
import Data.Colour
import Data.Colour.SRGB.Linear
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Godot.Core.GodotVisualServer as G
import qualified Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import qualified Godot.Methods as G
import Linear

import Plugin.Debug.DamagedRegions
import Plugin.Debug.DamagedRegionTypes
import Plugin.Debug.HudTypes
import Plugin.Debug.MemoryHudTypes
import Plugin.Debug.MonadoHudState (setDebugMonadoHudOpenXRFrameTimingActive)
import Plugin.Debug.MonadoHudTypes
import Plugin.Debug.ProfileHud
import Plugin.Debug.ProfileHudTypes
import Plugin.Imports
import Plugin.Types

data DebugHudGeometries =
  DebugHudGeometries
    { debugHudGeometriesBackgroundGeometry :: CanvasBaseGeometry
    , debugHudGeometriesTabBarGeometry     :: CanvasBaseGeometry
    , debugHudGeometriesContentGeometry    :: CanvasBaseGeometry
    }

newDebugHudFont :: Int -> IO GodotDynamicFont
newDebugHudFont fontSize = do
  debugFont <- unsafeInstance GodotDynamicFont "DynamicFont"
  dynamicFontData' <- load GodotDynamicFontData "DynamicFontData" "res://OpenSansEmoji.ttf"
  let dynamicFontData = Maybe.fromJust dynamicFontData'
  G.set_font_data debugFont dynamicFontData
  G.set_size debugFont fontSize
  return debugFont

getDebugHudReservedHeight :: IO Int
getDebugHudReservedHeight = do
  state <- getDebugHudRuntimeState
  if not (debugHudRuntimeVisible state)
    then return 0
    else
      return $
        debugHudTabBarHeight
          + debugHudPaddingPixels * 2
          + debugHudBaseMessageHeight
          + debugHudModeReservedContentHeight (debugHudRuntimeActiveMode state)

debugHudModeReservedContentHeight :: DebugHudMode -> Int
debugHudModeReservedContentHeight DebugHudProfile = debugProfileHudHeight
debugHudModeReservedContentHeight DebugHudMonado = debugMonadoHudHeight
debugHudModeReservedContentHeight DebugHudMemory = debugMemoryHudHeight
debugHudModeReservedContentHeight DebugHudDepthFirstSurfaces = debugDepthFirstThumbnailHeight
debugHudModeReservedContentHeight DebugHudDamagedRegions = debugDamagedRegionThumbnailHeight
debugHudModeReservedContentHeight _ = 0

toggleDebugHud :: GodotSimulaServer -> IO ()
toggleDebugHud gss = do
  debugHudToggleVisible
  syncDebugMonadoHudOpenXRFrameTimingActive
  markAllDebugHudViewsForRedraw gss

selectDebugHudMode :: GodotSimulaServer -> DebugHudMode -> IO ()
selectDebugHudMode gss mode = do
  clearAllDebugHudMessages gss
  debugHudSetMode mode
  syncDebugMonadoHudOpenXRFrameTimingActive
  markAllDebugHudViewsForRedraw gss

closeDebugHud :: GodotSimulaServer -> IO ()
closeDebugHud gss = do
  debugHudClose
  syncDebugMonadoHudOpenXRFrameTimingActive
  markAllDebugHudViewsForRedraw gss

syncDebugMonadoHudOpenXRFrameTimingActive :: IO ()
syncDebugMonadoHudOpenXRFrameTimingActive = do
  state <- getDebugHudRuntimeState
  setDebugMonadoHudOpenXRFrameTimingActive $
    debugHudRuntimeVisible state
      && debugHudRuntimeActiveMode state == DebugHudMonado

-- Right now: all GSVS HUDs are forced to be in one state (we might need to change this later).
-- So this function is used for HUD state changes, where tell all other gsvs to fully redraw (to synchronize HUD states).
markAllDebugHudViewsForRedraw :: GodotSimulaServer -> IO ()
markAllDebugHudViewsForRedraw gss = do
  views <- readTVarIO (gss ^. gssViews)
  mapM_ markGSVSForFullRedrawsByDefaultFrameAmount views

clearAllDebugHudMessages :: GodotSimulaServer -> IO ()
clearAllDebugHudMessages gss = do
  views <- readTVarIO (gss ^. gssViews)
  atomically $ do
    writeTVar debugHudGlobalMessages []
    forM_ views $ \gsvs -> do
      writeTVar (gsvs ^. gsvsDebugHudMessages) []
      writeTVar (gsvs ^. gsvsDebugHudLastPointerFocusSummary) Nothing

handleDebugHudClick :: GodotSimulaViewSprite -> CanvasBaseCoordinates -> Bool -> Int -> IO Bool
handleDebugHudClick gsvs coords pressed buttonIndex =
  if buttonIndex /= 1
    then return False
    else do
      state <- getDebugHudRuntimeState
      if not (debugHudRuntimeVisible state)
        then return False
        else do
          gss <- readTVarIO (gsvs ^. gsvsServer)
          hudGeometry <- getDebugHudGeometry gsvs
          case debugHudControlAt hudGeometry coords of
            Nothing ->
              if debugHudRuntimeActiveMode state == DebugHudProfile
                then handleDebugProfileHudClick gss gsvs coords pressed
                else return False
            Just DebugHudControlClose -> do
              when pressed $
                closeDebugHud gss
              return True
            Just (DebugHudControlTab mode) -> do
              when pressed $
                selectDebugHudMode gss mode
              return True

handleDebugProfileHudClick :: GodotSimulaServer -> GodotSimulaViewSprite -> CanvasBaseCoordinates -> Bool -> IO Bool
handleDebugProfileHudClick gss gsvs coords pressed = do
  snapshot <- getDebugProfileHudSnapshot
  profileGeometry <- getDebugHudProfileGeometry gsvs snapshot
  case debugProfileHudControlAt snapshot profileGeometry coords of
    Nothing -> return False
    Just control -> do
      when pressed $ do
        changed <- handleDebugProfileHudControl control
        when changed $
          markAllDebugHudViewsForRedraw gss
      return True

-- Returns the geometry for the whole "content area" of the of the Profile HUD
getDebugHudProfileGeometry :: GodotSimulaViewSprite -> DebugProfileHudSnapshot -> IO CanvasBaseGeometry
getDebugHudProfileGeometry gsvs snapshot = do
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
  viewportBase <- readTVarIO (cb ^. cbViewport)
  viewportSurface <- readTVarIO (cs ^. csViewport)
  V2 _ viewportSurfaceHeight <- G.get_size viewportSurface >>= fromLowLevel :: IO (V2 Float)
  V2 viewportBaseWidth viewportBaseHeight <- G.get_size viewportBase >>= fromLowLevel :: IO (V2 Float)
  let padding = fromIntegral debugHudPaddingPixels
  let profileRows = max 1 $ List.length $ debugProfileHudVisibleRows snapshot
  let requestedProfileHeight = fromIntegral $ debugProfileHudHeightForRows profileRows
  let availableHudHeight = max 0 (viewportBaseHeight - viewportSurfaceHeight)
  let availableProfileHeight =
        max 0 $
          availableHudHeight
            - fromIntegral debugHudTabBarHeight
            - padding * 2
  return $
    CanvasBaseGeometry
      { canvasBaseGeometryOffsetRight = OffsetRight padding
      , canvasBaseGeometryOffsetDown =
          OffsetDown $
            viewportSurfaceHeight
              + fromIntegral debugHudTabBarHeight
              + padding
      , canvasBaseGeometryWidth = Width (max 0 $ viewportBaseWidth - padding * 2)
      , canvasBaseGeometryHeight = Height (min requestedProfileHeight availableProfileHeight)
      }

getDebugHudGeometry :: GodotSimulaViewSprite -> IO CanvasBaseGeometry
getDebugHudGeometry gsvs = do
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
  viewportBase <- readTVarIO (cb ^. cbViewport)
  viewportSurface <- readTVarIO (cs ^. csViewport)
  V2 _ viewportSurfaceHeight <- G.get_size viewportSurface >>= fromLowLevel :: IO (V2 Float)
  V2 viewportBaseWidth _ <- G.get_size viewportBase >>= fromLowLevel :: IO (V2 Float)
  return $
    CanvasBaseGeometry
      { canvasBaseGeometryOffsetRight = OffsetRight 0
      , canvasBaseGeometryOffsetDown = OffsetDown viewportSurfaceHeight
      , canvasBaseGeometryWidth = Width viewportBaseWidth
      , canvasBaseGeometryHeight = Height (fromIntegral debugHudTabBarHeight)
      }

debugHudControlAt :: CanvasBaseGeometry -> CanvasBaseCoordinates -> Maybe DebugHudControl
debugHudControlAt hudGeometry coords =
  if not (canvasBaseCoordinatesInsideGeometry coords hudGeometry)
    then Nothing
    else
      fmap fst $
        List.find
          (\(_, controlGeometry) -> canvasBaseCoordinatesInsideGeometry coords controlGeometry)
          (debugHudControlGeometries hudGeometry)

canvasBaseCoordinatesInsideGeometry :: CanvasBaseCoordinates -> CanvasBaseGeometry -> Bool
canvasBaseCoordinatesInsideGeometry coords geometry =
  x >= left && x <= left + width && y >= top && y <= top + height
  where
    (x, y) = canvasBaseCoordinatesTuple coords
    (left, top, width, height) = canvasBaseGeometryTuple geometry

debugHudControlGeometries :: CanvasBaseGeometry -> [(DebugHudControl, CanvasBaseGeometry)]
debugHudControlGeometries hudGeometry =
  tabRects ++ [(DebugHudControlClose, closeRect)]
  where
    (_, hudTop, hudWidth, _) = canvasBaseGeometryTuple hudGeometry
    closeLeft = max debugHudTabInset (hudWidth - debugHudTabInset - debugHudCloseButtonSize)
    controlTop = hudTop + 4
    controlHeight = fromIntegral debugHudTabBarHeight - 6
    closeRect =
      CanvasBaseGeometry
        { canvasBaseGeometryOffsetRight = OffsetRight closeLeft
        , canvasBaseGeometryOffsetDown = OffsetDown controlTop
        , canvasBaseGeometryWidth = Width debugHudCloseButtonSize
        , canvasBaseGeometryHeight = Height controlHeight
        }
    tabAreaLeft = debugHudTabInset
    tabAreaRight = max tabAreaLeft (closeLeft - debugHudTabGap)
    tabCount = max 1 (List.length debugHudModes)
    tabWidth =
      max 1 $
        (tabAreaRight - tabAreaLeft - debugHudTabGap * fromIntegral (tabCount - 1))
          / fromIntegral tabCount
    tabRects =
      fmap
        (\(index, mode) ->
          ( DebugHudControlTab mode
          , CanvasBaseGeometry
              { canvasBaseGeometryOffsetRight = OffsetRight (tabAreaLeft + fromIntegral index * (tabWidth + debugHudTabGap))
              , canvasBaseGeometryOffsetDown = OffsetDown controlTop
              , canvasBaseGeometryWidth = Width tabWidth
              , canvasBaseGeometryHeight = Height controlHeight
              }
          )
        )
        (zip [0 :: Int ..] debugHudModes)

drawDebugHudTabs :: CanvasBase -> GodotDynamicFont -> DebugHudMode -> CanvasBaseGeometry -> IO ()
drawDebugHudTabs cb debugFont activeMode hudGeometry = do
  barRect <- canvasBaseGeometryToGodotRect2 hudGeometry
  barColor <- (toLowLevel $ (rgb 0.04 0.045 0.05) `withOpacity` 0.96) :: IO GodotColor
  G.draw_rect cb barRect barColor True 1.0 False

  inactiveFill <- (toLowLevel $ (rgb 0.12 0.13 0.15) `withOpacity` 0.96) :: IO GodotColor
  activeFill <- (toLowLevel $ (rgb 0.23 0.25 0.29) `withOpacity` 1.0) :: IO GodotColor
  inactiveBorder <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.16) :: IO GodotColor
  activeBorder <- (toLowLevel $ (rgb 0.38 0.70 1.0) `withOpacity` 1.0) :: IO GodotColor
  inactiveText <- (toLowLevel $ (rgb 0.78 0.82 0.86) `withOpacity` 1.0) :: IO GodotColor
  activeText <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
  closeFill <- (toLowLevel $ (rgb 0.18 0.07 0.07) `withOpacity` 0.96) :: IO GodotColor
  closeBorder <- (toLowLevel $ (rgb 1.0 0.45 0.45) `withOpacity` 0.9) :: IO GodotColor

  forM_ (debugHudControlGeometries hudGeometry) $ \(control, controlGeometry) ->
    case control of
      DebugHudControlClose -> do
        drawDebugHudControlGeometry cb controlGeometry closeFill closeBorder
        drawDebugHudCenteredText cb debugFont "X" activeText controlGeometry
      DebugHudControlTab mode -> do
        let active = mode == activeMode
        let (_, _, width, _) = canvasBaseGeometryTuple controlGeometry
        drawDebugHudControlGeometry cb controlGeometry (if active then activeFill else inactiveFill) (if active then activeBorder else inactiveBorder)
        label <- fitDebugHudLabel debugFont width mode
        drawDebugHudCenteredLines cb debugFont label (if active then activeText else inactiveText) controlGeometry

drawDebugHudControlGeometry :: CanvasBase -> CanvasBaseGeometry -> GodotColor -> GodotColor -> IO ()
drawDebugHudControlGeometry cb geometry fillColor borderColor = do
  rect <- canvasBaseGeometryToGodotRect2 geometry
  G.draw_rect cb rect fillColor True 1.0 False
  G.draw_rect cb rect borderColor False 1.0 False

drawDebugHudCenteredText :: CanvasBase -> GodotDynamicFont -> String -> GodotColor -> CanvasBaseGeometry -> IO ()
drawDebugHudCenteredText cb debugFont text color geometry =
  drawDebugHudCenteredLines cb debugFont [text] color geometry

drawDebugHudCenteredLines :: CanvasBase -> GodotDynamicFont -> [String] -> GodotColor -> CanvasBaseGeometry -> IO ()
drawDebugHudCenteredLines cb debugFont texts color geometry = do
  fontHeight <- G.get_height (safeCast debugFont :: GodotFont)
  fontAscent <- G.get_ascent (safeCast debugFont :: GodotFont)
  let (left, top, width, height) = canvasBaseGeometryTuple geometry
  let textInset = 4
  let lineGap = 0
  let availableWidth = max 1 (width - textInset * 2)
  let lineStep = fontHeight + lineGap
  let blockHeight = fontHeight * fromIntegral (List.length texts) + lineGap * fromIntegral (max 0 (List.length texts - 1))
  let firstBaseline = top + max fontAscent ((height - blockHeight) / 2 + fontAscent)
  forM_ (zip [0 :: Int ..] texts) $ \(lineIndex, text) ->
    drawDebugHudCenteredLine cb debugFont text color (left + textInset) (firstBaseline + lineStep * fromIntegral lineIndex) availableWidth

drawDebugHudCenteredLine :: CanvasBase -> GodotDynamicFont -> String -> GodotColor -> Float -> Float -> Float -> IO ()
drawDebugHudCenteredLine cb debugFont text color left baseline availableWidth =
  bracket
    (toLowLevel (pack text) :: IO GodotString)
    Api.godot_string_destroy
    (\textStr -> do
      V2 textWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) textStr >>= fromLowLevel :: IO (V2 Float)
      let x = left + max 0 ((availableWidth - textWidth) / 2)
      renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
      G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr color (round availableWidth))

fitDebugHudLabel :: GodotDynamicFont -> Float -> DebugHudMode -> IO [String]
fitDebugHudLabel debugFont width mode =
  firstFitting (debugHudModeLabelCandidates mode)
  where
    contentWidth = max 1 (width - 8)
    firstFitting [] = return []
    firstFitting [label] = return label
    firstFitting (label : rest) = do
      fits <- debugHudLabelFits debugFont contentWidth label
      if fits
        then return label
        else firstFitting rest

debugHudLabelFits :: GodotDynamicFont -> Float -> [String] -> IO Bool
debugHudLabelFits debugFont contentWidth labelLines =
  all (<= contentWidth) <$> mapM (measureDebugHudLabel debugFont) labelLines

measureDebugHudLabel :: GodotDynamicFont -> String -> IO Float
measureDebugHudLabel debugFont label =
  bracket
    (toLowLevel (pack label) :: IO GodotString)
    Api.godot_string_destroy
    (\labelStr -> do
      V2 labelWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) labelStr >>= fromLowLevel :: IO (V2 Float)
      return labelWidth)

debugHudModeLabelCandidates :: DebugHudMode -> [[String]]
debugHudModeLabelCandidates DebugHudProfile = [["Profile"]]
debugHudModeLabelCandidates DebugHudMonado = [["Monado", "Frame Timings"], ["Monado"], ["M"]]
debugHudModeLabelCandidates DebugHudMemory = [["Memory"], ["Mem"]]
debugHudModeLabelCandidates DebugHudSurfaceBoundaries = [["Surface", "Boundaries"], ["Surface", "Bounds"], ["Bounds"]]
debugHudModeLabelCandidates DebugHudSurfaceCreations = [["Surface", "Creations"], ["Surface", "Create"], ["Create"]]
debugHudModeLabelCandidates DebugHudMouseEvents = [["Mouse", "Events"], ["Mouse"]]
debugHudModeLabelCandidates DebugHudKeyboardEvents = [["Keyboard", "Events"], ["Keys", "Events"], ["Keys"]]
debugHudModeLabelCandidates DebugHudDepthFirstSurfaces = [["Depth First", "Surfaces"], ["Depth", "Surfaces"], ["Depth First"], ["Depth"]]
debugHudModeLabelCandidates DebugHudDamagedRegions = [["Damaged", "Regions"], ["Damage", "Regions"], ["Damage"]]
