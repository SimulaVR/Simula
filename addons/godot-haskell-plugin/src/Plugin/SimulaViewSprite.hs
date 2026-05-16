{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Data.Text hiding (show)
import Control.Exception
import Data.Proxy

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotVisualServer as G
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody    as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods                as G
import qualified Godot.Gdnative.Internal.Api  as Api
import           Godot.Nativescript

import Plugin.CanvasBase
import Plugin.CanvasSurface
import Plugin.Debug.DamagedRegions
import Plugin.Debug.DamagedRegionTypes
import Plugin.Debug.HUD
import Plugin.Debug.ProfileHudTypes
import Plugin.Types
import Data.Maybe
import Data.Either

-- import           Data.Vector.V2

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable
import qualified Dhall

import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Ordered as MO

import System.Process

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance NativeScript GodotSimulaViewSprite where
  className = "SimulaViewSprite"
  classInit obj =
    do GodotSimulaViewSprite (safeCast obj)
                      <$> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar 1.0)
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar (Nothing, Nothing))
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar 0)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar 0)
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar 0)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar Nothing)
  -- classExtends = "RigidBody"
  classMethods =
    [ func NoRPC "_input_event" (catchGodot inputEvent)
    , func NoRPC "_ready" (catchGodot ready)
    , func NoRPC "_handle_destroy" (catchGodot _handle_destroy)
    , func NoRPC "_handle_map" (catchGodot _handle_map)
    , func NoRPC "_process" (catchGodot Plugin.SimulaViewSprite._process)
    , func NoRPC "handle_unmap" (catchGodot handle_unmap)
    , func NoRPC "handle_unmap_child" (catchGodot handle_unmap_child)
    , func NoRPC "handle_unmap_free_child" (catchGodot handle_unmap_free_child)
    , func NoRPC "handle_map_free_child" (catchGodot handle_map_free_child)
    , func NoRPC "handle_map_child" (catchGodot handle_map_child)
    , func NoRPC "handle_set_parent" (catchGodot handle_set_parent)
    , func NoRPC "handle_xdg_set_parent" (catchGodot handle_xdg_set_parent)
    , func NoRPC "handle_new_popup" (catchGodot handle_new_popup)
    , func NoRPC "handle_window_menu" (catchGodot handle_window_menu)
    , func NoRPC "handle_wlr_surface_new_subsurface" (catchGodot handle_wlr_surface_new_subsurface)
    , func NoRPC "handle_wlr_surface_destroy" (catchGodot handle_wlr_surface_destroy)
    , func NoRPC "handle_wlr_surface_commit" (catchGodot handle_wlr_surface_commit)
    , func NoRPC "handle_wlr_subsurface_destroy" (catchGodot handle_wlr_subsurface_destroy)
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "map_free_child" [("wlrXWaylandSurface", GodotVariantTypeObject)]
                 ]



-- How many frames to wait before attempting to place a default/starting app
startingAppPlacementStableFrameCount :: Int
startingAppPlacementStableFrameCount = 2

-- How many frames to wait for a launched app's size to stabilize before allowing compensatory
-- centering (which keeps it centered). If we don't do this, some starting apps (like gvim, which
-- changes size a few frames in a row) will trigger compensatory movement, causing them to launch in
-- weird locations.
launchResizeCompensationGraceFrameCount :: Int
launchResizeCompensationGraceFrameCount = 12


-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = profileScope "Plugin.SimulaViewSprite.updateSimulaViewSprite" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.updateSimulaViewSprite"
  setTargetDimensions gsvs
  applyViewportBaseTexture gsvs
  setBoxShapeExtentsToMatchAABB gsvs

  whenM (spriteReadyToMove gsvs) $ do
      simulaView <- readTVarIO (gsvs ^. gsvsView) --
      let eitherSurface = (simulaView ^. svWlrEitherSurface)
      gss <- readTVarIO (gsvs ^. gsvsServer)
      maybeWindowClass <- case eitherSurface of
        Left wlrXdgSurface -> do
          wlrXdgSurface <- validateSurfaceE wlrXdgSurface
          getXdgAppId wlrXdgSurface
        Right wlrXWaylandSurface -> do
          wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
          getXWaylandWindowClass wlrXWaylandSurface
      maybeLocationByLaunchToken <- case (maybeWindowClass >>= extractStartingAppLaunchToken) of
        Nothing -> return Nothing
        Just launchToken -> getSimulaStartingLocationByLaunchToken gss launchToken
      maybeLocationByWindowClass <- case maybeLocationByLaunchToken of
        Just _ -> return Nothing
        Nothing -> case maybeWindowClass of
          Nothing -> return Nothing
          Just windowClass -> getSimulaStartingLocationByWindowClass gss windowClass
      maybeLocation <- case (maybeLocationByLaunchToken, maybeLocationByWindowClass) of
        (Just location, _) -> return (Just location)
        (Nothing, Just location) -> return (Just location)
        (Nothing, Nothing) -> do
          pid <- case eitherSurface of
            Left wlrXdgSurface -> do
              wlrXdgSurface <- validateSurfaceE wlrXdgSurface
              pidInt <- G.get_pid wlrXdgSurface
              return $ (fromInteger $ fromIntegral pidInt)
            Right wlrXWaylandSurface -> do
              wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
              pidInt <- G.get_pid wlrXWaylandSurface
              return $ (fromInteger $ fromIntegral pidInt)
          pids <- (pid:) <$> getParentsPids pid
          getSimulaStartingLocationByPid gss pids
      let resolutionPath = case (maybeLocationByLaunchToken, maybeLocationByWindowClass) of
            (Just _, _) -> "launch-token"
            (Nothing, Just _) -> "window-class"
            (Nothing, Nothing) -> "pid"
      appendWindowLog $ "[placement] path=" ++ resolutionPath ++ " class=" ++ show maybeWindowClass ++ " location=" ++ show maybeLocation
      case maybeLocation of
        Just location -> moveToStartingPosition gsvs location
        Nothing -> return ()
      atomically $ do
        writeTVar (_gsvsShouldMove gsvs) False
        writeTVar (gsvs ^. gsvsLaunchPlacementStableDims) Nothing
  where -- Necessary for window manipulation to function
        setBoxShapeExtentsToMatchAABB :: GodotSimulaViewSprite -> IO ()
        setBoxShapeExtentsToMatchAABB gsvs = profileScope "Plugin.SimulaViewSprite.updateSimulaViewSprite.setBoxShapeExtentsToMatchAABB" $ do
          debugPutStrLn "Plugin.SimulaViewSprite.setBoxShapeExtentsToMatchAABB"
          meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
          aabb <- G.get_aabb meshInstance
          size <- godot_aabb_get_size aabb
          shape <- atomically $ readTVar (_gsvsShape gsvs)

          -- Compute new extents
          size' <- godot_vector3_operator_divide_scalar size 2

          -- Set the box's "half extents"; in order to set the extents to `e`, you must `set_extents e/2`.
          -- https://docs.godotengine.org/en/stable/classes/class_boxshape.html
          G.set_extents shape size'

spriteReadyToMove :: GodotSimulaViewSprite -> IO Bool
spriteReadyToMove gsvs = profileScope "Plugin.SimulaViewSprite.spriteReadyToMove" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.spriteReadyToMove"
  shouldMove <- atomically $ readTVar (_gsvsShouldMove gsvs)
  if shouldMove then do meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
                        aabb <- G.get_aabb meshInstance
                        size <- godot_aabb_get_size aabb
                        vsize <- fromLowLevel size
                        simulaView <- readTVarIO (gsvs ^. gsvsView)
                        visibleDims <- getVisibleSurfaceDimensions (simulaView ^. svWlrEitherSurface)
                        stableDims <- atomically $ do
                          oldStableDims <- readTVar (gsvs ^. gsvsLaunchPlacementStableDims)
                          let newStableDims = case oldStableDims of
                                Just (oldDims, oldCount) | oldDims == visibleDims -> (visibleDims, oldCount + 1)
                                _ -> (visibleDims, 1)
                          writeTVar (gsvs ^. gsvsLaunchPlacementStableDims) (Just newStableDims)
                          return newStableDims
                        let aabbReady = vsize > 0 -- The first frame or so, the sprite has vsize 0
                        let dimsReady = snd stableDims >= startingAppPlacementStableFrameCount
                        return (aabbReady && dimsReady)
                   else return False

moveToStartingPosition :: GodotSimulaViewSprite -> String -> IO ()
moveToStartingPosition gsvs appLocation = profileScope "Plugin.SimulaViewSprite.moveToStartingPosition" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.moveToStartingPosition"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  meshInstance <- atomically $ readTVar (gsvs ^. gsvsMeshInstance)
  aabb   <- G.get_aabb meshInstance
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
  let sizeY  = size ^. _y
  startingAppTransform <- readTVarIO (gss ^. gssStartingAppTransform)
  startingAppTransform' <- case startingAppTransform of
    Nothing -> do
      gsvsTransform <- G.get_global_transform gsvs
      atomically $ writeTVar (gss ^. gssStartingAppTransform) (Just gsvsTransform)
      return gsvsTransform
    Just startingAppTransform' -> return startingAppTransform'
  case (appLocation) of
    "center" -> moveSpriteAlongObjectZAxis gsvs 0.3
    "right" -> do G.set_global_transform gsvs startingAppTransform'
                  G.translate gsvs =<< toLowLevel (V3 (-sizeX) 0 0)
    "bottom" -> do G.set_global_transform gsvs startingAppTransform'
                   G.translate gsvs =<< toLowLevel (V3 0 (-sizeY) 0)
    "left" -> do G.set_global_transform gsvs startingAppTransform'
                 G.translate gsvs =<< toLowLevel (V3 (sizeX) 0 0)
    "top" -> do G.set_global_transform gsvs startingAppTransform'
                G.translate gsvs =<< toLowLevel (V3 0 (sizeY) 0)
    _ -> return ()
  orientSpriteTowardsGaze gsvs

-- Sets gsvs wlr_xwayland_surface size and all associated viewports to the
-- gsvsTargetSize every frame
setTargetDimensions :: GodotSimulaViewSprite -> IO ()
setTargetDimensions gsvs = profileScope "Plugin.SimulaViewSprite.setTargetDimensions" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.setTargetDimensions"
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  renderTargetBase <- readTVarIO (cb ^. cbViewport)
  cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
  renderTargetSurface <- readTVarIO (cs ^. csViewport)

  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)

  -- Get state
  originalDims@(originalWidth, originalHeight) <- case eitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      V2 (V2 posX posY) (V2 xdgWidth xdgHeight) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
      return (round xdgWidth, round xdgHeight)
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      xwHeight <- G.get_height wlrXWaylandSurface
      xwWidth <- G.get_width wlrXWaylandSurface
      return (xwWidth, xwHeight)

  maybeTargetDims <- readTVarIO (gsvs ^. gsvsTargetSize)
  targetDims@(SpriteDimensions (targetWidth, targetHeight)) <- case maybeTargetDims of
        Nothing -> do
          atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (originalWidth, originalHeight)))
          return (SpriteDimensions originalDims)
        Just targetDims' -> return targetDims'

  -- Try to avoid forcing small popups to be large squares.
  let settledDimensions@(settledWidth, settledHeight) = if (originalWidth > 450 || originalHeight > 450)
        then (targetWidth, targetHeight)
        else (originalWidth, originalHeight)
  settledDimensions' <- toLowLevel $ (V2 (fromIntegral settledWidth) (fromIntegral settledHeight))

  spilloverDims@(spilloverWidth, spilloverHeight) <- getSpilloverDims gsvs
  debugHudReservedHeightActive <- getDebugHudReservedHeight
  let baseViewportHeightIncludingPotentialHud =
        spilloverHeight
          + debugHudReservedHeightActive
  spilloverDims' <- toLowLevel $ (V2 (fromIntegral spilloverWidth) (fromIntegral spilloverHeight))
  baseViewportDims' <- toLowLevel $ (V2 (fromIntegral spilloverWidth) (fromIntegral baseViewportHeightIncludingPotentialHud))
  V2 currentViewportWidth currentViewportHeight <- G.get_size renderTargetSurface >>= fromLowLevel :: IO (V2 Float)
  V2 currentBaseViewportWidth currentBaseViewportHeight <- G.get_size renderTargetBase >>= fromLowLevel :: IO (V2 Float)
  let viewportDimsChanged =
        ((round currentViewportWidth) /= spilloverWidth)
          || ((round currentViewportHeight) /= spilloverHeight)
          || ((round currentBaseViewportWidth) /= spilloverWidth)
          || ((round currentBaseViewportHeight) /= baseViewportHeightIncludingPotentialHud)

  -- Set buffer dimensions to new target size
  case eitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      toplevel <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
      G.set_size toplevel settledDimensions'
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      G.set_size wlrXWaylandSurface settledDimensions'

  -- Set the corresponding Viewports to match our new target size
  G.set_size renderTargetBase baseViewportDims'
  G.set_size renderTargetSurface spilloverDims'
  when viewportDimsChanged $
    markGSVSForFullRedrawsByDefaultFrameAmount gsvs
  -- QuadMesh need to be scaled down by a factor of 0.001 to be reasonably sized in Godot:
  withQuadMesh gsvs $ \quadMesh -> do
    -- The quad must match CanvasBase's full size, including the HUD reserve.
    -- The 0.001 factor converts pixel-like viewport dimensions into Godot mesh units.
    G.set_size quadMesh =<< (toLowLevel $ (V2 (0.001 * (fromIntegral spilloverWidth)) (0.001 * (fromIntegral baseViewportHeightIncludingPotentialHud))))
    -- QuadMesh size grows around its center. Shift the mesh center by half the
    -- added HUD height so the app surface stays visually anchored and the extra
    -- height appears below it.
    G.set_center_offset quadMesh =<< (toLowLevel $ V3 0 (-0.5 * 0.001 * fromIntegral debugHudReservedHeightActive) 0)

  -- Problem: Calls to `G.set_size quadMesh` cause gsvs to "jitter" (since
  -- they're resized from the center). We thus have to translate them to create
  -- the illusion that they're staying still. Unfortunately, doing so can cause
  -- weird rotational effects due to reasons that are unclear to me. We thus go
  -- through a the ritual of avoiding translating windows when they are being
  -- purposely resized; we also avoid calls to `orientSpriteTowardsGaze` when
  -- resizing windows.
  maybeSpilloverDimsOld <- readTVarIO (gsvs ^. gsvsSpilloverDims)
  resizedLastFrame <- readTVarIO (gsvs ^. gsvsResizedLastFrame)
  transOld <- readTVarIO (gsvs ^. gsvsTransparency)
  launchPlacementPending <- readTVarIO (gsvs ^. gsvsShouldMove)
  launchResizeGraceFrames <- atomically $ do
    frames <- readTVar (gsvs ^. gsvsLaunchResizeCompensationGraceFrames)
    when (frames > 0) $
      writeTVar (gsvs ^. gsvsLaunchResizeCompensationGraceFrames) (frames - 1)
    return frames
  case maybeSpilloverDimsOld of
    Nothing -> do
      if (spilloverWidth > 0) then atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims) else return ()
    Just spilloverDimsOld@(oldSpilloverWidth, oldSpilloverHeight) -> do
      case ((oldSpilloverWidth /= spilloverWidth) || (oldSpilloverHeight /= spilloverHeight), resizedLastFrame) of
        (False, _) -> return ()
        (True, True) -> do atomically $ do writeTVar (gsvs ^. gsvsResizedLastFrame) False
                                           writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                           when (transOld == 1) $
                             markGSVSForFullRedrawsByDefaultFrameAmount gsvs
        (True, False) -> do let pushX = spilloverWidth - oldSpilloverWidth
                            let pushY = spilloverHeight - oldSpilloverHeight
                            -- Don't engage in compensatory translation when an app first launches, because sometimes they change in size on launch for a few frames.
                            if (launchPlacementPending || launchResizeGraceFrames > 0)
                              then do
                                atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                              else do
                                pushBackVector <- toLowLevel (V3 (-0.5 * 0.001 * (fromIntegral pushX)) (-0.5 * 0.001 * (fromIntegral pushY)) 0) :: IO GodotVector3
                                G.translate_object_local gsvs pushBackVector
                                atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                                case (transOld == 1, (spilloverWidth > targetWidth || spilloverHeight > targetHeight)) of
                                  (True, False) ->  return () -- Avoid changing shader when apps first launch
                                  (True, True) -> markGSVSForFullRedrawsByDefaultFrameAmount gsvs
                                  (False, _) -> return ()
  updateQuadShader gsvs targetDims spilloverDims
  return ()

updateQuadShader :: GodotSimulaViewSprite -> SpriteDimensions -> (Int, Int) -> IO ()
updateQuadShader gsvs (SpriteDimensions (targetWidth, targetHeight)) (spilloverWidth, spilloverHeight) = profileScope "Plugin.SimulaViewSprite.updateQuadShader" $ do
  gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
  fullRedrawFramesRemaining <- readTVarIO (gsvs ^. gsvsFullRedrawFramesRemaining)
  surfaceNeedsAlphaBlend <- gsvsViewOrChildrenHaveInsetGeometry gsvs
  debugHudActive <- debugHudEnabled
  let shouldUseTransparentShader =
        (gsvsTransparency < 1)
        || (fullRedrawFramesRemaining > 0)
        || (spilloverWidth /= targetWidth) -- Use the transparent shader whenever the rendered
        || (spilloverHeight /= targetHeight) -- surface does not fill the target quad.
        || surfaceNeedsAlphaBlend
        || debugHudActive
  setShader gsvs $
    if shouldUseTransparentShader
      then "res://addons/godot-haskell-plugin/TextShader.tres"
      else "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"

ready :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
ready self gvArgs = profileScope "Plugin.SimulaViewSprite.ready" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.ready"
  -- putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

inputEvent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
inputEvent self gvArgs@[_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] = profileScope "Plugin.SimulaViewSprite.inputEvent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.inputEvent"
  ev <- fromGodotVariant evObj
  clickPos <- fromGodotVariant clickPosObj >>= worldCoordinates3DFromGodotVector
  processInputEvent self ev clickPos
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> WorldCoordinates3D -> IO ()
processInputEvent gsvs ev clickPos = profileScope "Plugin.SimulaViewSprite.processInputEvent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.processInputEvent"
  -- putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processPointerMotionEvent gsvs clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processPointerButtonEvent gsvs (Button pressed button)
    return ()

processPointerMotionEvent :: GodotSimulaViewSprite -> WorldCoordinates3D -> IO ()
processPointerMotionEvent gsvs clickPos = profileScope "Plugin.SimulaViewSprite.processPointerMotionEvent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.processPointerMotionEvent"
  surfaceLocalCoords <- getCanvasBaseCoordinatesFromWorldHit gsvs clickPos
  debugPrintPointerSeatState gsvs Motion surfaceLocalCoords
  processClickEvent' gsvs Motion surfaceLocalCoords

processPointerButtonEvent :: GodotSimulaViewSprite -> InputEventType -> IO ()
processPointerButtonEvent gsvs evt = profileScope "Plugin.SimulaViewSprite.processPointerButtonEvent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.processPointerButtonEvent"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  surfaceLocalCoords <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
  handledByDebugHud <- case evt of
    Button pressed buttonIndex -> handleDebugHudClick gsvs surfaceLocalCoords pressed buttonIndex
    Motion -> return False
  unless handledByDebugHud $ do
    debugPrintPointerSeatState gsvs evt surfaceLocalCoords
    maybeSerial <- notifyMouseButtonWithMaybeSerial wlrSeat evt
    debugMouseEventsActive <- debugMouseEventsEnabled
    when debugMouseEventsActive $
      mapM_
        (\serial ->
          putStrLn $
            "Mouse button delivery serial="
              ++ show serial)
        maybeSerial
    pointerNotifyFrame wlrSeat

newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = profileScope "Plugin.SimulaViewSprite.newGodotSimulaViewSprite" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.newGodotSimulaViewSprite"
  gsvsObj <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' [] :: IO GodotObject
  maybeGSVS <- asNativeScript gsvsObj :: IO (Maybe GodotSimulaViewSprite)
  let gsvs = Data.Maybe.fromJust maybeGSVS


  meshInstance <- unsafeInstance GodotMeshInstance "MeshInstance"
  quadMesh <- unsafeInstance GodotQuadMesh "QuadMesh"
  G.set_size quadMesh =<< (toLowLevel $ (V2 0 0)) -- Initialize to (0,0) size or we'll experience "flickering" when apps launch
  G.set_mesh meshInstance (safeCast quadMesh)
  G.add_child gsvs (safeCast meshInstance) True

  godotBoxShape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast godotBoxShape)

   -- Load default cursor
  maybeCursorTexture <- readTVarIO (gss ^. gssCursorTexture)
  atomically $ writeTVar (_gsvsCursor gsvs) (Nothing, maybeCursorTexture)

  atomically $ writeTVar (_gsvsServer            gsvs) gss
  atomically $ writeTVar (_gsvsMeshInstance      gsvs) meshInstance
  atomically $ writeTVar (_gsvsShape             gsvs) godotBoxShape
  atomically $ writeTVar (_gsvsView              gsvs) simulaView
  atomically $ writeTVar (_gsvsCursorCoordinates gsvs) (CanvasBaseCoordinates (RightCoordinate 0) (DownCoordinate 0))

  -- Set config settings
  configuration <- readTVarIO (gss ^. gssConfiguration)
  let windowScale = realToFrac (configuration ^. defaultWindowScale) :: Float
  (V3 1 1 1 ^* (windowScale)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)

  -- Remove until order independent transparency is implemented
  -- let defaultTransparency' = constrainTransparency $ realToFrac (configuration ^. defaultTransparency)
  atomically $ writeTVar (_gsvsTransparency      gsvs) 1

  let maybeWindowResolution = (configuration ^. defaultWindowResolution) :: Maybe (Dhall.Natural, Dhall.Natural)
  case maybeWindowResolution of
    Just windowResolution'@(x, y) -> do atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (fromIntegral x, fromIntegral y)))
    Nothing -> return () -- If we don't have a target size, we delay setting QuadMesh size until we're able to retrieve its buffer dimensions

  setShader gsvs "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"
  G.set_process gsvs False

  return gsvs

replaceActiveSurface :: GodotSimulaViewSprite -> Maybe GodotWlrSurface -> IO ()
replaceActiveSurface gsvs maybeWlrSurface = profileScope "Plugin.SimulaViewSprite.replaceActiveSurface" $ do
  maybePreviousSurface <- atomically $ do
    previousSurface <- readTVar (gsvs ^. gsvsActiveSurface)
    writeTVar (gsvs ^. gsvsActiveSurface) maybeWlrSurface
    return previousSurface
  mapM_ (destroyMaybe . safeCast) maybePreviousSurface

clearActiveSurface :: GodotSimulaViewSprite -> IO ()
clearActiveSurface gsvs = profileScope "Plugin.SimulaViewSprite.clearActiveSurface" $
  replaceActiveSurface gsvs Nothing

replaceCursorSurface :: GodotSimulaViewSprite -> Maybe GodotWlrSurface -> IO ()
replaceCursorSurface gsvs maybeWlrSurface = profileScope "Plugin.SimulaViewSprite.replaceCursorSurface" $ do
  maybePreviousSurface <- atomically $ do
    (previousSurface, maybeCursorTexture) <- readTVar (gsvs ^. gsvsCursor)
    writeTVar (gsvs ^. gsvsCursor) (maybeWlrSurface, maybeCursorTexture)
    return previousSurface
  mapM_ (destroyMaybe . safeCast) maybePreviousSurface

clearCursorSurface :: GodotSimulaViewSprite -> IO ()
clearCursorSurface gsvs = profileScope "Plugin.SimulaViewSprite.clearCursorSurface" $
  replaceCursorSurface gsvs Nothing

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = profileScope "Plugin.SimulaViewSprite.focus" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.focus"
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView)
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)

  previousActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  when (previousActiveCursorGSVS /= Just gsvs) $
    mapM_ clearActiveSurface previousActiveCursorGSVS

  atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) (Just gsvs)
  atomically $ writeTVar (gss ^. gssActiveCursorGSVS) (Just gsvs)

  case wlrEitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      withGodotRef (G.get_wlr_surface wlrXdgSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
        wlrSurface <- validateSurfaceE wlrSurface
        G.reference wlrSurface
        replaceActiveSurface gsvs (Just wlrSurface)
        toplevel <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
        G.set_activated toplevel True
        debugPrintKeyboardFocusChange gsvs "focus xdg root keyboard_notify_enter" (Just wlrSurface)
        G.keyboard_notify_enter wlrSeat wlrSurface
        pointerNotifyEnter gsvs wlrSeat wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate 0) (DownCoordinate 0))
        pointerNotifyFrame wlrSeat
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
        wlrSurface <- validateSurfaceE wlrSurface
        G.reference wlrSurface
        replaceActiveSurface gsvs (Just wlrSurface)
        safeSetActivated gsvs True -- G.set_activated wlrXWaylandSurface True
        debugPrintKeyboardFocusChange gsvs "focus xwayland root keyboard_notify_enter" (Just wlrSurface)
        G.keyboard_notify_enter wlrSeat wlrSurface
        pointerNotifyEnter gsvs wlrSeat wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate 0) (DownCoordinate 0))
        pointerNotifyFrame wlrSeat

-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> WorldCoordinates3D
                  -> IO ()
processClickEvent gsvs evt clickPos = profileScope "Plugin.SimulaViewSprite.processClickEvent" $ do
  surfaceLocalCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) <- getCanvasBaseCoordinatesFromWorldHit gsvs clickPos
  processClickEvent' gsvs evt surfaceLocalCoords

processClickEvent' :: GodotSimulaViewSprite
                  -> InputEventType
                  -> CanvasBaseCoordinates
                  -> IO ()
processClickEvent' gsvs evt surfaceLocalCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.processClickEvent'" $ do
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  handledByDebugHud <- case evt of
    Button pressed buttonIndex -> handleDebugHudClick gsvs surfaceLocalCoords pressed buttonIndex
    Motion -> return False

  unless handledByDebugHud $ do
    isMapped <- readTVarIO $ (simulaView ^. svMapped)
    case isMapped of
      False -> putStrLn "processClickevent' isMapped is false!"
      True -> do let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
                 maybeSurfaceAndCoords <-
                   case wlrEitherSurface of
                     Right godotWlrXWaylandSurface -> do
                       fmap (\(wlrSurface, coords, focusOwnerXWaylandSurface) -> (wlrSurface, coords, Nothing, Just focusOwnerXWaylandSurface)) <$>
                         getXWaylandSubsurfaceAndCoordsWithKeyboardFocus gsvs godotWlrXWaylandSurface surfaceLocalCoords
                     Left godotWlrXdgSurface ->
                       fmap (\(wlrSurface, coords, focusedXdgSurface) -> (wlrSurface, coords, Just focusedXdgSurface, Nothing)) <$>
                         getXdgSubsurfaceAndCoordsAttachedAware gsvs godotWlrXdgSurface surfaceLocalCoords
                 case maybeSurfaceAndCoords of
                   Nothing -> do
                     wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
                     clearActiveSurface gsvs
                     debugHudClearPointerFocus gsvs
                     G.pointer_clear_focus wlrSeat
                     case evt of
                       Motion -> return ()
                       Button _ _ -> pointerNotifyButton wlrSeat evt
                     pointerNotifyFrame wlrSeat
                   Just (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)), maybeFocusedXdgSurface, maybeFocusOwnerXWaylandSurface) ->
                     (do
                       wlrSeat <- readTVarIO (gss ^. gssWlrSeat)

                       case evt of
                         Motion -> do
                           pointerNotifyEnter gsvs wlrSeat godotWlrSurface subSurfaceLocalCoords
                           pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                         Button _ _ -> do
                           -- Keyboard focus/activation goes to the owning surface for this hit.
                           -- Pointer events go to the concrete wlr_surface returned by surface_at,
                           -- which may be a subsurface/leaf (i.e. parented xdg toplevel or xwayland
                           -- free child) rather than the owning surface itself.
                           mapM_ (keyboardFocusAnXdgRootSurfaceFromGSVS gsvs) maybeFocusedXdgSurface
                           mapM_ (keyboardFocusXWaylandRootWlrSurfaceFromGSVS gsvs) maybeFocusOwnerXWaylandSurface
                           pointerNotifyEnter gsvs wlrSeat godotWlrSurface subSurfaceLocalCoords
                           pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                           debugPrintMouseButtonIntercept gsvs evt godotWlrSurface surfaceLocalCoords subSurfaceLocalCoords
                           pointerNotifyButton wlrSeat evt

                       pointerNotifyFrame wlrSeat)
                       `finally` destroyMaybe (safeCast godotWlrSurface)


debugPrintMouseButtonIntercept :: GodotSimulaViewSprite -> InputEventType -> GodotWlrSurface -> CanvasBaseCoordinates -> SubSurfaceLocalCoordinates -> IO ()
debugPrintMouseButtonIntercept gsvs inputEventType wlrSurface (CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) (SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)) = profileScope "Plugin.SimulaViewSprite.debugPrintMouseButtonIntercept" $ do
  debugMouseEventsActive <- debugMouseEventsEnabled
  when debugMouseEventsActive $ do
    case inputEventType of
      Motion -> return ()
      Button pressed buttonIndex -> do
        (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
        let msg =
              "Mouse button intercepted by surface="
                ++ show wlrSurface
                ++ " button="
                ++ show buttonIndex
                ++ " pressed="
                ++ show pressed
                ++ " surfaceLocal=("
                ++ show sx
                ++ ","
                ++ show sy
                ++ ") subSurfaceLocal=("
                ++ show ssx
                ++ ","
                ++ show ssy
                ++ ") buffer="
                ++ show bufferWidth
                ++ "x"
                ++ show bufferHeight
        putStrLn msg
        debugHudPush gsvs $
          "MOUSE button surface="
            ++ show wlrSurface
            ++ " button="
            ++ show buttonIndex
            ++ " pressed="
            ++ show pressed
            ++ " local=("
            ++ show ssx
            ++ ","
            ++ show ssy
            ++ ") buffer="
            ++ show bufferWidth
            ++ "x"
            ++ show bufferHeight

debugPrintPointerSeatState :: GodotSimulaViewSprite -> InputEventType -> CanvasBaseCoordinates -> IO ()
debugPrintPointerSeatState gsvs inputEventType surfaceLocalCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.debugPrintPointerSeatState" $ do
  debugMouseEventsActive <- debugMouseEventsEnabled
  when debugMouseEventsActive $ do
    case inputEventType of
      Motion -> do
        gss <- readTVarIO (gsvs ^. gsvsServer)
        wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
        focusedSurfaceSummary <- getFocusedSurfaceSummary wlrSeat
        hitSurfaceSummary <- getHitSurfaceSummary gsvs surfaceLocalCoords
        putStrLn $
          "Mouse motion seat state surfaceLocal=("
            ++ show sx
            ++ ","
            ++ show sy
            ++ ") focused="
            ++ focusedSurfaceSummary
            ++ " freshHit="
            ++ hitSurfaceSummary
      Button pressed buttonIndex -> do
        gss <- readTVarIO (gsvs ^. gsvsServer)
        wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
        focusedSurfaceSummary <- getFocusedSurfaceSummary wlrSeat
        hitSurfaceSummary <- getHitSurfaceSummary gsvs surfaceLocalCoords
        let msg =
              "Mouse button seat state button="
                ++ show buttonIndex
                ++ " pressed="
                ++ show pressed
                ++ " surfaceLocal=("
                ++ show sx
                ++ ","
                ++ show sy
                ++ ") focused="
                ++ focusedSurfaceSummary
                ++ " freshHit="
                ++ hitSurfaceSummary
        putStrLn msg
        debugHudPush gsvs $
          "MOUSE seat button="
            ++ show buttonIndex
            ++ " pressed="
            ++ show pressed
            ++ " focused="
            ++ focusedSurfaceSummary
            ++ " hit="
            ++ hitSurfaceSummary
  where
    getFocusedSurfaceSummary :: GodotWlrSeat -> IO String
    getFocusedSurfaceSummary wlrSeat =
      profileScope "Plugin.SimulaViewSprite.debugPrintPointerSeatState.getFocusedSurfaceSummary" $
      withGodotRef (G.get_pointer_focused_surface wlrSeat :: IO GodotWlrSurface) $ \focusedSurface ->
        case validateObject focusedSurface of
          Nothing -> return "nothing"
          Just validFocusedSurface -> do
            validFocusedSurface <- validateSurfaceE validFocusedSurface
            (bufferWidth, bufferHeight) <- getBufferDimensions validFocusedSurface
            return $
              "surface="
                ++ show validFocusedSurface
                ++ " buffer="
                ++ show bufferWidth
                ++ "x"
                ++ show bufferHeight

    getHitSurfaceSummary :: GodotSimulaViewSprite -> CanvasBaseCoordinates -> IO String
    getHitSurfaceSummary gsvs' coords = profileScope "Plugin.SimulaViewSprite.debugPrintPointerSeatState.getHitSurfaceSummary" $ do
      simulaView <- readTVarIO (gsvs' ^. gsvsView)
      let wlrEitherSurface = simulaView ^. svWlrEitherSurface
      maybeSurfaceAndCoords <-
        case wlrEitherSurface of
          Right godotWlrXWaylandSurface ->
            getXWaylandSubsurfaceAndCoords gsvs' godotWlrXWaylandSurface coords
          Left godotWlrXdgSurface -> do
            maybeHit <- getXdgSubsurfaceAndCoordsAttachedAware gsvs' godotWlrXdgSurface coords
            return $ fmap (\(wlrSurface, subCoords, _) -> (wlrSurface, subCoords)) maybeHit
      case maybeSurfaceAndCoords of
        Nothing -> return "nothing"
        Just (godotWlrSurface, SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)) ->
          (`finally` destroyMaybe (safeCast godotWlrSurface)) $ do
            (bufferWidth, bufferHeight) <- getBufferDimensions godotWlrSurface
            return $
              "surface="
                ++ show godotWlrSurface
                ++ " subSurfaceLocal=("
                ++ show ssx
                ++ ","
                ++ show ssy
                ++ ") buffer="
                ++ show bufferWidth
                ++ "x"
                ++ show bufferHeight

debugPrintKeyboardFocusChange :: GodotSimulaViewSprite -> String -> Maybe GodotWlrSurface -> IO ()
debugPrintKeyboardFocusChange gsvs context maybeTargetSurface = profileScope "Plugin.SimulaViewSprite.debugPrintKeyboardFocusChange" $ do
  debugKeyboardEventsActive <- debugKeyboardEventsEnabled
  when debugKeyboardEventsActive $ do
    focusSummary <- debugDescribeKeyboardFocus gsvs
    targetSurfaceSummary <- describeMaybeWlrSurface maybeTargetSurface
    let msg =
          "Keyboard focus change context="
            ++ context
            ++ " targetSurface="
            ++ targetSurfaceSummary
            ++ " "
            ++ focusSummary
    putStrLn msg
    debugHudPush gsvs $
      "KEY focus context="
        ++ context
        ++ " target="
        ++ targetSurfaceSummary
        ++ " "
        ++ focusSummary

debugDescribeKeyboardFocus :: GodotSimulaViewSprite -> IO String
debugDescribeKeyboardFocus gsvs = profileScope "Plugin.SimulaViewSprite.debugDescribeKeyboardFocus" $ do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  maybeKeyboardFocusedGSVS <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
  maybeActiveSurface <- readTVarIO (gsvs ^. gsvsActiveSurface)
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  pointerFocusedSurfaceSummary <- getPointerFocusedSurfaceSummary wlrSeat
  activeSurfaceSummary <- describeMaybeWlrSurface maybeActiveSurface
  viewSummary <- describeViewSurface simulaView
  return $
    "focus="
      ++ describeMaybeGSVS maybeKeyboardFocusedGSVS
      ++ " focusMatches="
      ++ show (maybeKeyboardFocusedGSVS == Just gsvs)
      ++ " activeSurface="
      ++ activeSurfaceSummary
      ++ " pointerFocus="
      ++ pointerFocusedSurfaceSummary
      ++ " view="
      ++ viewSummary

describeGSVS :: GodotSimulaViewSprite -> String
describeGSVS gsvs = "gsvsObj=" ++ show (_gsvsObj gsvs)

describeMaybeGSVS :: Maybe GodotSimulaViewSprite -> String
describeMaybeGSVS Nothing = "nothing"
describeMaybeGSVS (Just gsvs) = describeGSVS gsvs

debugPrintXWaylandFreeChildKeyboardFocus :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
debugPrintXWaylandFreeChildKeyboardFocus gsvs freeChildSurface wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y)) = profileScope "Plugin.SimulaViewSprite.debugPrintXWaylandFreeChildKeyboardFocus" $ do
  debugKeyboardEventsActive <- debugKeyboardEventsEnabled
  when debugKeyboardEventsActive $ do
    pid <- G.get_pid freeChildSurface
    surfaceX <- G.get_surface_origin_x freeChildSurface
    surfaceY <- G.get_surface_origin_y freeChildSurface
    width <- G.get_width freeChildSurface
    height <- G.get_height freeChildSurface
    focusSummary <- debugDescribeKeyboardFocus gsvs
    let msg =
          "Keyboard focus xwayland free child activated child="
            ++ show freeChildSurface
            ++ " pid="
            ++ show pid
            ++ " geometry=("
            ++ show surfaceX
            ++ ","
            ++ show surfaceY
            ++ " "
            ++ show width
            ++ "x"
            ++ show height
            ++ ") childLocal=("
            ++ show x
            ++ ","
            ++ show y
            ++ ") wlrSurface="
            ++ show wlrSurface
            ++ " "
            ++ focusSummary
    putStrLn msg
    debugHudPush gsvs $
      "KEY free-child focus child="
        ++ show freeChildSurface
        ++ " pid="
        ++ show pid
        ++ " geom=("
        ++ show surfaceX
        ++ ","
        ++ show surfaceY
        ++ " "
        ++ show width
        ++ "x"
        ++ show height
        ++ ") wlrSurface="
        ++ show wlrSurface

describeMaybeWlrSurface :: Maybe GodotWlrSurface -> IO String
describeMaybeWlrSurface Nothing = profileScope "Plugin.SimulaViewSprite.describeMaybeWlrSurface" $ return "nothing"
describeMaybeWlrSurface (Just wlrSurface) = profileScope "Plugin.SimulaViewSprite.describeMaybeWlrSurface" $ do
  validWlrSurface <- validateSurfaceE wlrSurface
  (bufferWidth, bufferHeight) <- getBufferDimensions validWlrSurface
  return $
    "surface="
      ++ show validWlrSurface
      ++ " buffer="
      ++ show bufferWidth
      ++ "x"
      ++ show bufferHeight

getPointerFocusedSurfaceSummary :: GodotWlrSeat -> IO String
getPointerFocusedSurfaceSummary wlrSeat = profileScope "Plugin.SimulaViewSprite.getPointerFocusedSurfaceSummary" $
  withGodotRef (G.get_pointer_focused_surface wlrSeat :: IO GodotWlrSurface) $ \focusedSurface ->
    case validateObject focusedSurface of
      Nothing -> return "nothing"
      Just validFocusedSurface -> describeMaybeWlrSurface (Just validFocusedSurface)

describeViewSurface :: SimulaView -> IO String
describeViewSurface simulaView = profileScope "Plugin.SimulaViewSprite.describeViewSurface" $
  case simulaView ^. svWlrEitherSurface of
    Left wlrXdgSurface -> do
      pid <- G.get_pid wlrXdgSurface
      roleInt <- G.get_role wlrXdgSurface
      return $
        "protocol=xdg surface="
          ++ show wlrXdgSurface
          ++ " pid="
          ++ show pid
          ++ " role="
          ++ xdgRoleName roleInt
    Right wlrXWaylandSurface -> do
      pid <- G.get_pid wlrXWaylandSurface
      x <- G.get_surface_origin_x wlrXWaylandSurface
      y <- G.get_surface_origin_y wlrXWaylandSurface
      width <- G.get_width wlrXWaylandSurface
      height <- G.get_height wlrXWaylandSurface
      return $
        "protocol=xwayland surface="
          ++ show wlrXWaylandSurface
          ++ " pid="
          ++ show pid
          ++ " geometry=("
          ++ show x
          ++ ","
          ++ show y
          ++ " "
          ++ show width
          ++ "x"
          ++ show height
          ++ ")"

xdgRoleName :: Int -> String
xdgRoleName 0 = "none"
xdgRoleName 1 = "toplevel"
xdgRoleName 2 = "popup"
xdgRoleName role = "unknown(" ++ show role ++ ")"

debugPrintWlrSurfaceMapDetails :: String -> GodotWlrSurface -> IO ()
debugPrintWlrSurfaceMapDetails prefix wlrSurface = profileScope "Plugin.SimulaViewSprite.debugPrintWlrSurfaceMapDetails" $ do
  debugSurfaceCreationsActive <- debugSurfaceCreationsEnabled
  when debugSurfaceCreationsActive $ do
    (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
    let msg =
          prefix
            ++ " surface="
            ++ show wlrSurface
            ++ " buffer="
            ++ show bufferWidth
            ++ "x"
            ++ show bufferHeight
    putStrLn msg
    debugHudPushGlobal msg

debugPrintXWaylandMapDetails :: String -> GodotWlrXWaylandSurface -> IO ()
debugPrintXWaylandMapDetails prefix wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.debugPrintXWaylandMapDetails" $ do
  debugSurfaceCreationsActive <- debugSurfaceCreationsEnabled
  when debugSurfaceCreationsActive $ do
    x <- G.get_surface_origin_x wlrXWaylandSurface
    y <- G.get_surface_origin_y wlrXWaylandSurface
    width <- G.get_width wlrXWaylandSurface
    height <- G.get_height wlrXWaylandSurface
    pid <- G.get_pid wlrXWaylandSurface
    let msg =
          prefix
            ++ " protocol=xwayland pid="
            ++ show pid
            ++ " surface="
            ++ show wlrXWaylandSurface
            ++ " geometry=("
            ++ show x
            ++ ","
            ++ show y
            ++ " "
            ++ show width
            ++ "x"
            ++ show height
            ++ ")"
    putStrLn msg
    debugHudPushGlobal msg

debugPrintXdgMapDetails :: String -> GodotWlrXdgSurface -> IO ()
debugPrintXdgMapDetails prefix wlrXdgSurface = profileScope "Plugin.SimulaViewSprite.debugPrintXdgMapDetails" $ do
  debugSurfaceCreationsActive <- debugSurfaceCreationsEnabled
  when debugSurfaceCreationsActive $ do
    roleInt <- G.get_role wlrXdgSurface
    pid <- G.get_pid wlrXdgSurface
    V2 (V2 posX posY) (V2 width height) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
    withGodotRef (G.get_wlr_surface wlrXdgSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
      (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
      let msg =
            prefix
              ++ " protocol=xdg role="
              ++ xdgRoleName roleInt
              ++ " pid="
              ++ show pid
              ++ " surface="
              ++ show wlrXdgSurface
              ++ " geometry=("
              ++ show posX
              ++ ","
              ++ show posY
              ++ " "
              ++ show width
              ++ "x"
              ++ show height
              ++ ") buffer="
              ++ show bufferWidth
              ++ "x"
              ++ show bufferHeight
      putStrLn msg
      debugHudPushGlobal msg

debugPrintCurrentMappedSurface :: String -> GodotSimulaViewSprite -> IO ()
debugPrintCurrentMappedSurface prefix gsvs = profileScope "Plugin.SimulaViewSprite.debugPrintCurrentMappedSurface" $ do
  debugSurfaceCreationsActive <- debugSurfaceCreationsEnabled
  when debugSurfaceCreationsActive $ do
    simulaView <- readTVarIO (gsvs ^. gsvsView)
    case simulaView ^. svWlrEitherSurface of
      Left wlrXdgSurface -> do
        wlrXdgSurface <- validateSurfaceE wlrXdgSurface
        debugPrintXdgMapDetails prefix wlrXdgSurface
      Right wlrXWaylandSurface -> do
        wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
        debugPrintXWaylandMapDetails prefix wlrXWaylandSurface

-- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
-- | TODO: This function just returns parent surface/coords. Fix!
-- | TODO: Use _xdg_surface_at*
getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> CanvasBaseCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getXdgSubsurfaceAndCoords wlrXdgSurface cursorCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.getXdgSubsurfaceAndCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getXdgSubsurfaceAndCoords"
  rect2@(V2 (V2 posX posY) (V2 xdgWidth xdgHeight)) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
  -- surface_at expects coordinates relative to the top level wlr_surface, so you don't need to worry about geometry rect offsets or anything
  withGodotRef (G.surface_at wlrXdgSurface sx sy :: IO GodotWlrSurfaceAtResult) $ \wlrSurfaceAtResult -> do
    withGodotRef (G.get_surface wlrSurfaceAtResult :: IO GodotWlrSurface) $ \wlrSurfaceSubSurface -> do
      case validateObject wlrSurfaceSubSurface of
        Nothing -> return Nothing
        Just validWlrSurfaceSubSurface -> do
          validWlrSurfaceSubSurface <- validateSurfaceE validWlrSurfaceSubSurface
          G.reference validWlrSurfaceSubSurface
          ssx <- G.get_sub_x wlrSurfaceAtResult
          ssy <- G.get_sub_y wlrSurfaceAtResult
          let ssCoordinates = SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)
          return $ Just (validWlrSurfaceSubSurface, ssCoordinates)

getXdgSubsurfaceAndCoordsAttachedAware :: GodotSimulaViewSprite -> GodotWlrXdgSurface -> CanvasBaseCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXdgSurface))
getXdgSubsurfaceAndCoordsAttachedAware gsvs rootWlrXdgSurface compositionCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.getXdgSubsurfaceAndCoordsAttachedAware" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getXdgSubsurfaceAndCoordsAttachedAware"
  (offsetX, offsetY) <- getGSVSSurfacesCoordinateOffsetFromOrigin gsvs
  let rootLocalCoords =
        CanvasBaseCoordinates
          (RightCoordinate (sx - fromIntegral offsetX))
          (DownCoordinate (sy - fromIntegral offsetY))
  maybeAttachedHit <- getAttachedXdgSubsurfaceAndCoords gsvs rootLocalCoords
  case maybeAttachedHit of
    Just hit -> return $ Just hit
    Nothing -> do
      maybeRootHit <- getXdgSubsurfaceAndCoords rootWlrXdgSurface rootLocalCoords
      return $ fmap (\(wlrSurface, coords) -> (wlrSurface, coords, rootWlrXdgSurface)) maybeRootHit

getAttachedXdgSubsurfaceAndCoords :: GodotSimulaViewSprite -> CanvasBaseCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXdgSurface))
getAttachedXdgSubsurfaceAndCoords gsvs surfaceLocalCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.getAttachedXdgSubsurfaceAndCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getAttachedXdgSubsurfaceAndCoords"
  attachedChildren <- readTVarIO (gsvs ^. gsvsAttachedXdgChildren)
  findAttachedHit (Data.List.reverse attachedChildren)
  where
    findAttachedHit [] = return Nothing
    findAttachedHit (childWlrXdgSurface:remainingChildren) = do
      (childRootX, childRootY) <- getAttachedXdgChildRootOffset gsvs childWlrXdgSurface
      let childLocalCoords =
            CanvasBaseCoordinates
              (RightCoordinate (sx - fromIntegral childRootX))
              (DownCoordinate (sy - fromIntegral childRootY))
      maybeChildHit <- getXdgSubsurfaceAndCoords childWlrXdgSurface childLocalCoords
      case maybeChildHit of
        Nothing -> findAttachedHit remainingChildren
        Just (wlrSurface, subSurfaceLocalCoords) ->
          return $ Just (wlrSurface, subSurfaceLocalCoords, childWlrXdgSurface)

keyboardNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> IO ()
keyboardNotifyEnter wlrSeat wlrSurface = profileScope "Plugin.SimulaViewSprite.keyboardNotifyEnter" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.keyboardNotifyEnter"
  G.keyboard_notify_enter wlrSeat wlrSurface

-- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
-- | need one since the GodotWlrSeat keeps internal track of what the currently
-- | active surface is.
pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)) = profileScope "Plugin.SimulaViewSprite.pointerNotifyMotion" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyMotion"
  G.pointer_notify_motion wlrSeat ssx ssy
  -- putStrLn $ "G.point_notify_motion: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
pointerNotifyButton wlrSeat inputEventType = profileScope "Plugin.SimulaViewSprite.pointerNotifyButton" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyButton"
  notifyMouseButtonWithMaybeSerial wlrSeat inputEventType >> return ()

notifyMouseButtonWithMaybeSerial :: GodotWlrSeat -> InputEventType -> IO (Maybe Int)
notifyMouseButtonWithMaybeSerial wlrSeat inputEventType = profileScope "Plugin.SimulaViewSprite.notifyMouseButtonWithMaybeSerial" $
  case inputEventType of
    Motion -> return Nothing
    Button pressed buttonIndex ->
      case buttonIndex of
        BUTTON_WHEEL_UP -> pointerNotifyButton' >> return Nothing
        BUTTON_WHEEL_DOWN -> pointerNotifyButton' >> return Nothing
        BUTTON_WHEEL_LEFT -> pointerNotifyButton' >> return Nothing
        BUTTON_WHEEL_RIGHT -> pointerNotifyButton' >> return Nothing
        _ -> Just <$> (G.pointer_notify_button wlrSeat (fromIntegral buttonIndex) pressed :: IO Int)
      where
        pointerNotifyButton' = do
          G.pointer_notify_button wlrSeat (fromIntegral buttonIndex) pressed
          return ()

-- | Sends a frame event to the surface with pointer focus (apparently
-- | useful in particular for axis events); unclear if this is needed in VR but we
-- | use it regardless.
pointerNotifyFrame :: GodotWlrSeat -> IO ()
pointerNotifyFrame wlrSeat = profileScope "Plugin.SimulaViewSprite.pointerNotifyFrame" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyFrame"
  G.pointer_notify_frame wlrSeat

-- | Let wlroots know we have entered a new surface. We can safely call this
-- | over and over (wlroots checks if we've called it already for this surface
-- | and, if so, returns early.
pointerNotifyEnter :: GodotSimulaViewSprite -> GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyEnter gsvs wlrSeat wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy)) = profileScope "Plugin.SimulaViewSprite.pointerNotifyEnter" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyEnter"
  debugMouseEventsActive <- debugMouseEventsEnabled
  when debugMouseEventsActive $
    debugHudPushPointerFocus gsvs ("surface=" ++ show wlrSurface)
  G.pointer_notify_enter wlrSeat wlrSurface ssx ssy -- Causing a crash

_handle_map :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_map gsvs _ = profileScope "Plugin.SimulaViewSprite._handle_map" $ do
  debugPutStrLn "Plugin.SimulaViewSprite._handle_map"
  debugPrintCurrentMappedSurface "Plugin.SimulaViewSprite._handle_map" gsvs
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      maybeParentGSVS <- getXdgParentGSVS gss wlrXdgSurface
      case maybeParentGSVS of
        Just parentGSVS -> mapAsAttachedXdgToplevel gsvs simulaView wlrXdgSurface parentGSVS
        Nothing -> mapAsStandaloneSurface gsvs simulaView eitherSurface
    Right _ ->
      mapAsStandaloneSurface gsvs simulaView eitherSurface

  return ()

mapAsStandaloneSurface :: GodotSimulaViewSprite -> SimulaView -> Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO ()
mapAsStandaloneSurface gsvs simulaView eitherSurface = profileScope "Plugin.SimulaViewSprite.mapAsStandaloneSurface" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.mapAsStandaloneSurface"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  case eitherSurface of
    Left wlrXdgSurface -> do
      validateSurfaceE wlrXdgSurface
      return ()
    Right wlrXWaylandSurface -> do -- Safeguard to prevent potentially weird behavior
      validateSurfaceE wlrXWaylandSurface
      zero <- toLowLevel (V2 0 0)
      G.set_xy wlrXWaylandSurface zero
      return ()
  G.set_process gsvs True
  G.set_process_input gsvs True
  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)
  atomically $ do
    writeTVar (_gsvsShouldMove gsvs) True
    writeTVar (gsvs ^. gsvsLaunchResizeCompensationGraceFrames) launchResizeCompensationGraceFrameCount

  -- Add the gsvs as a child to the current workspace
  (workspace, workspaceStr) <- readTVarIO (gss ^. gssWorkspace)
  G.add_child ((safeCast workspace) :: GodotNode)
              ((safeCast gsvs)      :: GodotNode)
              True

  -- We must add viewportSurface to the gsvs before the viewportBase,
  -- since sibling order affects draw order and the latter samples from the former
  cs <- newCanvasSurface gsvs
  viewportSurface <- readTVarIO (cs ^. csViewport)
  atomically $ writeTVar (gsvs ^. gsvsCanvasSurface) cs
  G.set_process cs True
  addChild gsvs viewportSurface
  addChild viewportSurface cs

  cb <- newCanvasBase gsvs
  viewportBase <- readTVarIO (cb ^. cbViewport)
  atomically $ writeTVar (gsvs ^. gsvsCanvasBase) cb
  G.set_process cb True
  addChild gsvs viewportBase
  addChild viewportBase cb

  surfaceIsParented <- surfaceHasParent eitherSurface
  if surfaceIsParented
    -- This hack is to accomodate parented XDG popups which spawn as toplevels and
    -- which for some reason generate weird artifacts unless we redraw 23 frames deep
    -- (according to one test). I'm setting to 30 in case other scenarios need more
    -- redraws. These popups seem to be rare (basically help menus), so in practice
    -- this path won't be used that often.
    then markGSVSForFullRedrawFrames gsvs 30
    else markGSVSForFullRedrawMilliseconds gsvs

  setInFrontOfUser gsvs (-2)

  V3 1 1 1 ^* (1 + 1 * 1) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)

  focus gsvs -- We're relying on this to add references to wlrSurface :/

  atomically $ writeTVar (simulaView ^. svMapped) True

  return ()

mapAsAttachedXdgToplevel :: GodotSimulaViewSprite -> SimulaView -> GodotWlrXdgSurface -> GodotSimulaViewSprite -> IO ()
mapAsAttachedXdgToplevel childGSVS simulaView childWlrXdgSurface parentGSVS = profileScope "Plugin.SimulaViewSprite.mapAsAttachedXdgToplevel" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.mapAsAttachedXdgToplevel"
  gss <- readTVarIO (childGSVS ^. gsvsServer)
  G.set_process childGSVS False
  G.set_process_input childGSVS False
  atomically $ do
    modifyTVar' (gss ^. gssViews) (M.insert simulaView childGSVS)
    writeTVar (simulaView ^. svMapped) True
    writeTVar (childGSVS ^. gsvsShouldMove) False
    writeTVar (childGSVS ^. gsvsIsAtTargetDims) True
  attachXdgChildToParent gss parentGSVS childWlrXdgSurface
  markGSVSForFullRedrawFrames parentGSVS 30
  keyboardFocusAnXdgRootSurfaceFromGSVS parentGSVS childWlrXdgSurface

attachXdgChildToParent :: GodotSimulaServer -> GodotSimulaViewSprite -> GodotWlrXdgSurface -> IO ()
attachXdgChildToParent gss parentGSVS childWlrXdgSurface = profileScope "Plugin.SimulaViewSprite.attachXdgChildToParent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.attachXdgChildToParent"
  attachedMap <- readTVarIO (gss ^. gssAttachedXdgChildren)
  case M.lookup childWlrXdgSurface attachedMap of
    Just oldParentGSVS | oldParentGSVS /= parentGSVS -> do
      detachXdgChildFromParent gss childWlrXdgSurface
      return ()
    _ -> return ()
  attachedChildren <- readTVarIO (parentGSVS ^. gsvsAttachedXdgChildren)
  let attachedChildrenNew = Data.List.delete childWlrXdgSurface attachedChildren ++ [childWlrXdgSurface]
  atomically $ do
    writeTVar (parentGSVS ^. gsvsAttachedXdgChildren) attachedChildrenNew
    modifyTVar' (gss ^. gssAttachedXdgChildren) (M.insert childWlrXdgSurface parentGSVS)

detachXdgChildFromParent :: GodotSimulaServer -> GodotWlrXdgSurface -> IO (Maybe GodotSimulaViewSprite)
detachXdgChildFromParent gss childWlrXdgSurface = profileScope "Plugin.SimulaViewSprite.detachXdgChildFromParent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.detachXdgChildFromParent"
  attachedMap <- readTVarIO (gss ^. gssAttachedXdgChildren)
  case M.lookup childWlrXdgSurface attachedMap of
    Nothing -> return Nothing
    Just parentGSVS -> do
      attachedChildren <- readTVarIO (parentGSVS ^. gsvsAttachedXdgChildren)
      let attachedChildrenNew = Data.List.delete childWlrXdgSurface attachedChildren
      atomically $ do
        writeTVar (parentGSVS ^. gsvsAttachedXdgChildren) attachedChildrenNew
        modifyTVar' (gss ^. gssAttachedXdgChildren) (M.delete childWlrXdgSurface)
      markGSVSForFullRedrawFrames parentGSVS 30
      return $ Just parentGSVS

detachXWaylandFreeChildFromParent :: GodotSimulaServer -> GodotWlrXWaylandSurface -> IO (Maybe GodotSimulaViewSprite)
detachXWaylandFreeChildFromParent gss childWlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.detachXWaylandFreeChildFromParent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.detachXWaylandFreeChildFromParent"
  freeChildrenMap <- readTVarIO (gss ^. gssFreeChildren)
  case M.lookup childWlrXWaylandSurface freeChildrenMap of
    Nothing -> return Nothing
    Just parentGSVS -> do
      freeChildren <- readTVarIO (parentGSVS ^. gsvsFreeChildren)
      let freeChildrenNew = Data.List.delete childWlrXWaylandSurface freeChildren
      atomically $ do
        writeTVar (parentGSVS ^. gsvsFreeChildren) freeChildrenNew
        modifyTVar' (gss ^. gssFreeChildren) (M.delete childWlrXWaylandSurface)
      markGSVSForFullRedrawFrames parentGSVS 30
      return $ Just parentGSVS

-- The second argument is the XDG root surface that should receive keyboard
-- focus/activation. It is either the XDG surface wrapped by the SimulaView, or
-- the root XDG surface of an attached parented XDG toplevel. We need this
-- because the GSVS being clicked and the XDG toplevel that should receive focus
-- are no longer always the same thing: clicking an attached child should focus
-- that child, while clicking the parent/root area should focus the SimulaView's
-- own XDG surface.
keyboardFocusAnXdgRootSurfaceFromGSVS :: GodotSimulaViewSprite -> GodotWlrXdgSurface -> IO ()
keyboardFocusAnXdgRootSurfaceFromGSVS spatialGSVS wlrXdgSurface = profileScope "Plugin.SimulaViewSprite.keyboardFocusAnXdgRootSurfaceFromGSVS" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.keyboardFocusAnXdgRootSurfaceFromGSVS"
  gss <- readTVarIO (spatialGSVS ^. gsvsServer)
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  previousActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  when (previousActiveCursorGSVS /= Just spatialGSVS) $
    mapM_ clearActiveSurface previousActiveCursorGSVS
  atomically $ do
    writeTVar (gss ^. gssKeyboardFocusedSprite) (Just spatialGSVS)
    writeTVar (gss ^. gssActiveCursorGSVS) (Just spatialGSVS)
  withGodotRef (G.get_wlr_surface wlrXdgSurface :: IO GodotWlrSurface) $ \rootWlrSurface -> do
    rootWlrSurface <- validateSurfaceE rootWlrSurface
    G.reference rootWlrSurface
    replaceActiveSurface spatialGSVS (Just rootWlrSurface)
    toplevel <- G.get_xdg_toplevel wlrXdgSurface >>= validateSurfaceE
    G.set_activated toplevel True
    debugPrintKeyboardFocusChange spatialGSVS "focus attached xdg root keyboard_notify_enter" (Just rootWlrSurface)
    G.keyboard_notify_enter wlrSeat rootWlrSurface

keyboardFocusXWaylandRootWlrSurfaceFromGSVS :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO ()
keyboardFocusXWaylandRootWlrSurfaceFromGSVS spatialGSVS wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.keyboardFocusXWaylandRootWlrSurfaceFromGSVS" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.keyboardFocusXWaylandRootWlrSurfaceFromGSVS"
  gss <- readTVarIO (spatialGSVS ^. gsvsServer)
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  previousActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  when (previousActiveCursorGSVS /= Just spatialGSVS) $
    mapM_ clearActiveSurface previousActiveCursorGSVS
  atomically $ do
    writeTVar (gss ^. gssKeyboardFocusedSprite) (Just spatialGSVS)
    writeTVar (gss ^. gssActiveCursorGSVS) (Just spatialGSVS)
  withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \rootWlrSurface -> do
    rootWlrSurface <- validateSurfaceE rootWlrSurface
    G.reference rootWlrSurface -- +1 the refe count since
    replaceActiveSurface spatialGSVS (Just rootWlrSurface) -- ...we store reference to it here
    G.set_activated wlrXWaylandSurface True
    debugPrintKeyboardFocusChange spatialGSVS "focus xwayland root keyboard_notify_enter" (Just rootWlrSurface)
    G.keyboard_notify_enter wlrSeat rootWlrSurface

surfaceHasParent :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO Bool
surfaceHasParent eitherSurface = profileScope "Plugin.SimulaViewSprite.surfaceHasParent" $
  case eitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      isJust <$> getXdgParentSurface wlrXdgSurface
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      parent <- G.get_parent wlrXWaylandSurface :: IO GodotWlrXWaylandSurface
      return $ isJust (validateObject parent)

handle_unmap :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap self args@[_xdgOrXwaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_unmap" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap"
  handle_unmap_base self args
  mapM_ Api.godot_variant_destroy args

-- Passes control entirely to updateSimulaViewSprite.
_process :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_process self gvArgs =
  profileScope "Plugin.SimulaViewSprite._process" $ processSimulaViewSprite self gvArgs

processSimulaViewSprite :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
processSimulaViewSprite self gvArgs = do
  debugPutStrLn "Plugin.SimulaViewSprite._process"
  simulaView <- readTVarIO (self ^. gsvsView)
  mapped <- atomically $ readTVar (simulaView ^. svMapped)
  when mapped $ do
    isAtTargetDims <- readTVarIO (self ^. gsvsIsAtTargetDims)
    isAtTargetDimsNow <-
      if isAtTargetDims
        then return True
        else resizeToTargetDimsIfBigEnough self
    atomically $ writeTVar (self ^. gsvsIsAtTargetDims) isAtTargetDimsNow

    -- First presentation of gsvs must not depend on configure-size acceptance.
    -- Some XDG popups commit real buffers while refusing Simula's requested
    -- target size, so they still need a nonzero QuadMesh/Viewport immediately.
    hasPresentableSize <- hasPresentableSurfaceSize self
    when (hasPresentableSize || isAtTargetDimsNow) $
      updateSimulaViewSprite self
  mapM_ Api.godot_variant_destroy gvArgs
  return ()
  where hasPresentableSurfaceSize :: GodotSimulaViewSprite -> IO Bool
        hasPresentableSurfaceSize gsvs = do
          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          case eitherSurface of
            Left wlrXdgSurface -> do
              wlrXdgSurface <- validateSurfaceE wlrXdgSurface
              V2 _ (V2 xdgWidth xdgHeight) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
              withGodotRef (G.get_wlr_surface wlrXdgSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
                (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
                return $
                  ((round xdgWidth) > 0 && (round xdgHeight) > 0)
                    || (bufferWidth > 0 && bufferHeight > 0)
            Right wlrXWaylandSurface -> do
              wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
              surfaceWidth <- G.get_width wlrXWaylandSurface
              surfaceHeight <- G.get_height wlrXWaylandSurface
              withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
                (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
                return $
                  (surfaceWidth > 0 && surfaceHeight > 0)
                    || (bufferWidth > 0 && bufferHeight > 0)

        resizeToTargetDimsIfBigEnough :: GodotSimulaViewSprite -> IO Bool
        resizeToTargetDimsIfBigEnough gsvs = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_commit.resizeToTargetDimsIfBigEnough" $ do
          cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
          renderTargetBase <- readTVarIO (cb ^. cbViewport)
          cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
          renderTargetSurface <- readTVarIO (cs ^. csViewport)

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)

          -- Get state
          originalDims@(originalWidth, originalHeight) <- case eitherSurface of
            Left wlrXdgSurface -> do
              validateSurfaceE wlrXdgSurface
              V2 (V2 posX posY) (V2 xdgWidth xdgHeight) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
              return (round xdgWidth, round xdgHeight)
            Right wlrXWaylandSurface -> do
              validateSurfaceE wlrXWaylandSurface
              xwHeight <- G.get_height wlrXWaylandSurface
              xwWidth <- G.get_width wlrXWaylandSurface
              return (xwWidth, xwHeight)

          maybeTargetDims <- readTVarIO (gsvs ^. gsvsTargetSize)
          targetDims@(SpriteDimensions (targetWidth, targetHeight)) <- case maybeTargetDims of
                Nothing -> do
                  atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (originalWidth, originalHeight)))
                  return (SpriteDimensions originalDims)
                Just targetDims' -> return targetDims'

          -- Try to avoid forcing small popups to be large squares.
          let settledDimensions@(settledWidth, settledHeight) = if (originalWidth > 450 || originalHeight > 450)
                then (targetWidth, targetHeight)
                else (originalWidth, originalHeight)
          settledDimensions' <- toLowLevel $ (V2 (fromIntegral settledWidth) (fromIntegral settledHeight))

          -- Set buffer dimensions to new target size
          case eitherSurface of
            Left wlrXdgSurface -> do
              validateSurfaceE wlrXdgSurface
              toplevel <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
              G.set_size toplevel settledDimensions'
            Right wlrXWaylandSurface -> do
              validateSurfaceE wlrXWaylandSurface
              G.set_size wlrXWaylandSurface settledDimensions'

          -- Try to avoid forcing small popups to be large squares.
          let isSmallPopUp = (originalWidth < 450 || originalHeight < 450)
          let isAtTargetDims = ((targetWidth == originalWidth) && (targetHeight == originalHeight))
          return (isSmallPopUp || isAtTargetDims)


_handle_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_destroy gsvs gvArgs@[_gsvsGV] = profileScope "Plugin.SimulaViewSprite._handle_destroy" $ do
  debugPutStrLn "Plugin.SimulaViewSprite._handle_destroy"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  case maybeActiveCursorGSVS of
    Just activeCursorGSVS -> case (gsvs == activeCursorGSVS) of
                                  True -> do atomically $ writeTVar (gss ^. gssActiveCursorGSVS) Nothing
                                  False -> return ()
    Nothing -> return ()

  clearActiveSurface gsvs
  clearCursorSurface gsvs
  case eitherSurface of
    Left wlrXdgSurface -> do
      detachXdgChildFromParent gss wlrXdgSurface
      attachedChildren <- readTVarIO (gsvs ^. gsvsAttachedXdgChildren)
      forM_ attachedChildren $ \childWlrXdgSurface ->
        detachXdgChildFromParent gss childWlrXdgSurface
    Right wlrXWaylandSurface -> do
      detachXWaylandFreeChildFromParent gss wlrXWaylandSurface
      freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
      forM_ freeChildren $ \childWlrXWaylandSurface ->
        detachXWaylandFreeChildFromParent gss childWlrXWaylandSurface
  G.queue_free gsvs -- Queue the `gsvs` for destruction
  G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
  atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
  deleteSurface eitherSurface -- Causing issues with rr
  mapM_ Api.godot_variant_destroy gvArgs

  where
    deleteSurface eitherSurface =
      case eitherSurface of
        (Left xdgSurface) -> destroyMaybe (safeCast xdgSurface)
        (Right xwaylandSurface) -> destroyMaybe (safeCast xwaylandSurface)

-- | Push the gsvs by `dist` units along its object-local z-axis. Negative values of `dist`
-- | push the gsvs away from the user; positive values of `dist` push the gsvs
-- | towards the user.
moveSpriteAlongObjectZAxis :: GodotSimulaViewSprite -> Float -> IO ()
moveSpriteAlongObjectZAxis gsvs dist = profileScope "Plugin.SimulaViewSprite.moveSpriteAlongObjectZAxis" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.moveSpriteAlongObjectZAxis"
  orientSpriteTowardsGaze gsvs
  pushBackVector <- toLowLevel (V3 0 0 dist) :: IO GodotVector3 -- For some reason we also have to shift the vector 0.5 units to the right
  G.translate_object_local gsvs pushBackVector
  return ()

setInFrontOfUser :: GodotSimulaViewSprite -> Float -> IO ()
setInFrontOfUser gsvs zAxisDist = profileScope "Plugin.SimulaViewSprite.setInFrontOfUser" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.setInFrontOfUser"
  gsvsScale <- G.get_scale (safeCast gsvs :: GodotSpatial)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  pushBackVector <- toLowLevel (V3 0 0 zAxisDist) :: IO GodotVector3
  hmdGlobalTransform <- getARVRCameraOrPancakeCameraTransformGlobal gss
  G.set_global_transform gsvs hmdGlobalTransform
  G.translate_object_local gsvs pushBackVector
  G.rotate_object_local gsvs rotationAxisY 3.14159
  G.scale_object_local (safeCast gsvs :: GodotSpatial) gsvsScale

safeSetActivated :: GodotSimulaViewSprite -> Bool -> IO ()
safeSetActivated gsvs active = profileScope "Plugin.SimulaViewSprite.safeSetActivated" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.safeSetActivated"
  simulaView <- readTVarIO $ (gsvs ^. gsvsView)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  case (isMapped, wlrEitherSurface) of
    (True, Right wlrXWaylandSurface) -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      G.set_activated wlrXWaylandSurface True
    (False, _) -> return ()
    (_, Left wlrXdgSurface) -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
      return ()
      -- G.set_activated toplevel True

applyViewportBaseTexture :: GodotSimulaViewSprite -> IO ()
applyViewportBaseTexture gsvs =
  profileScope "Plugin.SimulaViewSprite.applyViewportBaseTexture" $ applyViewportBaseTextureImpl gsvs

applyViewportBaseTextureImpl :: GodotSimulaViewSprite -> IO ()
applyViewportBaseTextureImpl gsvs = do
  debugPutStrLn "Plugin.SimulaViewSprite.applyViewportBaseTexture"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  viewportBase <- readTVarIO (cb ^. cbViewport)

  withQuadMesh gsvs $ \quadMesh ->
    withGodotRef (G.get_material quadMesh :: IO GodotMaterial) $ \material -> do
      shm <- asClass' GodotShaderMaterial "ShaderMaterial" material :: IO GodotShaderMaterial
      withGodotRef (G.get_texture viewportBase :: IO GodotViewportTexture) $ \viewportBaseTexture -> do
        viewportBaseTextureGV <- (toLowLevel (toVariant ((safeCast viewportBaseTexture) :: GodotObject))) :: IO GodotVariant
        texture_albedo <- toLowLevel (pack "texture_albedo") :: IO GodotString
        G.set_shader_param shm texture_albedo viewportBaseTextureGV
        Api.godot_variant_destroy viewportBaseTextureGV
        Api.godot_string_destroy texture_albedo

handle_map_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_map_free_child gsvsInvisible gvArgs@[wlrXWaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_map_free_child" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_map_free_child"
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  debugPrintXWaylandMapDetails "Plugin.SimulaViewSprite.handle_map_free_child" wlrXWaylandSurface
  surfaceWasMappedAndReferenced <- handle_map_free_child_impl gsvsInvisible wlrXWaylandSurface
  when surfaceWasMappedAndReferenced $ mapM_ Api.godot_variant_destroy gvArgs
  return ()

-- Returns True if the surface gets `G.reference`'ed
handle_map_free_child_impl :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO Bool
handle_map_free_child_impl gsvsInvisible wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.handle_map_free_child_impl" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_map_free_child_impl"
  debugPrintXWaylandMapDetails "Plugin.SimulaViewSprite.handle_map_free_child_impl" wlrXWaylandSurface
  gss <- readTVarIO $ (gsvsInvisible ^. gsvsServer)
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  surfaceMappedAndReferenced <- case maybeActiveCursorGSVS of
     Nothing -> do
       putStrLn "Cannot map free child!"
       return False
     Just gsvs -> do maybeSurfaceLocalCoords <- computeSurfaceLocalCoordinates gsvs wlrXWaylandSurface
                     case maybeSurfaceLocalCoords of
                       Nothing -> return False
                       Just (sx, sy) -> do -- Push surface onto end of gsvsFreeChildren stack
                                           x <- G.get_surface_origin_x wlrXWaylandSurface
                                           y <- G.get_surface_origin_y wlrXWaylandSurface
                                           let xOffset = (sx - x)
                                           let yOffset = (sy - y)

                                           -- adjustedXY <- getAdjustedXYFreeChild gsvs wlrXWaylandSurface
                                           -- adjustedXY'@(V2 x' y') <- fromLowLevel adjustedXY :: IO (V2 Float)
                                           -- G.set_xy wlrXWaylandSurface adjustedXY

                                           -- let sx' = (if (round x') == (fromIntegral x) then sx else (round x'))
                                           -- let sy' = (if (round y') == (fromIntegral y) then sy else (round y'))

                                           G.reference wlrXWaylandSurface
                                           freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
                                           atomically $ writeTVar (gsvs ^. gsvsFreeChildren) (freeChildren ++ [wlrXWaylandSurface])

                                           -- Add surface into gssFreeChildren map
                                           freeChildrenMapOld <- readTVarIO (gss ^. gssFreeChildren) :: IO (M.Map GodotWlrXWaylandSurface GodotSimulaViewSprite)
                                           let freeChildrenMapNew = M.insert wlrXWaylandSurface gsvs freeChildrenMapOld
                                           atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew
                                           markGSVSForFullRedrawsByDefaultFrameAmount gsvs
                                           return True

  simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
  atomically $ writeTVar (simulaView ^. svMapped) True
  return surfaceMappedAndReferenced
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_map.computeSurfaceLocalCoordinates" $ do
          debugPutStrLn "Plugin.SimulaViewSprite.computeSurfaceLocalCoordinates"
          localX <- G.get_surface_origin_x child
          localY <- G.get_surface_origin_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> do
                            validateSurfaceE wlrXdgSurface
                            return Nothing
                          Right parent -> do
                            validateSurfaceE parent
                            globalX <- G.get_surface_origin_x parent
                            globalY <- G.get_surface_origin_y parent
                            return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

getAdjustedXY :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO GodotVector2
getAdjustedXY gsvs wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getAdjustedXY" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getAdjustedXY"
  childSX <- G.get_surface_origin_x wlrXWaylandSurface
  childSY <- G.get_surface_origin_y wlrXWaylandSurface
  childWidth <- G.get_width wlrXWaylandSurface
  childHeight <- G.get_height wlrXWaylandSurface
  maybeSpriteDims <- readTVarIO (gsvs ^. gsvsTargetSize)

  (parentWidth, parentHeight) <- case maybeSpriteDims of
                     Nothing -> do
                        simulaView <- readTVarIO (gsvs ^. gsvsView)
                        let eitherSurface = (simulaView ^. svWlrEitherSurface)
                        withWlrSurface eitherSurface $ \wlrSurface ->
                          getBufferDimensions wlrSurface
                     Just (SpriteDimensions (parentWidth, parentHeight)) -> return (parentWidth, parentHeight)

  let overlapHeight = (childSY + childHeight) - parentHeight
  let overlapWidth = (childSX + childWidth) - parentWidth

  let adjustedX = case ((overlapWidth > 0), (childSX - overlapWidth) < 0) of
                       (True, True) -> childSX
                       (True, False) -> childSX - overlapWidth
                       (False, _) -> childSX
  let adjustedY = case ((overlapHeight > 0), (childSY - overlapHeight) < 0) of
                       (True, True) -> childSY
                       (True, False) -> childSY - overlapHeight
                       (False, _) -> childSY

  adjustedXY <- toLowLevel $ V2 (fromIntegral adjustedX) (fromIntegral adjustedY)
  adjustedXY'@(V2 x' y') <- fromLowLevel adjustedXY :: IO (V2 Float)
  case ((adjustedX + childWidth > parentWidth), (adjustedY + childHeight > parentHeight)) of
    (True, False) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (adjustedX + childWidth, parentHeight)))
    (False, True) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (parentWidth, adjustedY + childHeight)))
    (True, True) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (adjustedX + childWidth, adjustedY + childHeight)))
    _ -> return ()
  return adjustedXY


getAdjustedXYFreeChild :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO GodotVector2
getAdjustedXYFreeChild gsvs wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getAdjustedXYFreeChild" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getAdjustedXYFreeChild"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  (V2 parentX parentY) <- case eitherSurface of
                               (Left wlrXdgSurface) -> do
                                 wlrXdgSurface <- validateSurfaceE wlrXdgSurface
                                 return (V2 0 0)
                               (Right wlrXWaylandSurfaceParent) -> do
                                 wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurfaceParent
                                 x <- G.get_surface_origin_x wlrXWaylandSurfaceParent
                                 y <- G.get_surface_origin_y wlrXWaylandSurfaceParent
                                 return $ V2 x y
  childX <- G.get_surface_origin_x wlrXWaylandSurface
  childY <- G.get_surface_origin_y wlrXWaylandSurface

  let childSX = childX - parentX
  let childSY = childY - parentY

  childWidth <- G.get_width wlrXWaylandSurface
  childHeight <- G.get_height wlrXWaylandSurface
  maybeSpriteDims <- readTVarIO (gsvs ^. gsvsTargetSize)

  (parentWidth, parentHeight) <- case maybeSpriteDims of
                     Nothing -> do
                        simulaView <- readTVarIO (gsvs ^. gsvsView)
                        let eitherSurface = (simulaView ^. svWlrEitherSurface)
                        withWlrSurface eitherSurface $ \wlrSurface ->
                          getBufferDimensions wlrSurface
                     Just (SpriteDimensions (parentWidth, parentHeight)) -> return (parentWidth, parentHeight)

  let overlapHeight = (childSY + childHeight) - parentHeight
  let overlapWidth = (childSX + childWidth) - parentWidth

  let adjustedX = case ((overlapWidth > 0), (childSX - overlapWidth) < 0) of
                       (True, True) -> childSX
                       (True, False) -> childSX - overlapWidth
                       (False, _) -> childSX
  let adjustedY = case ((overlapHeight > 0), (childSY - overlapHeight) < 0) of
                       (True, True) -> childSY
                       (True, False) -> childSY - overlapHeight
                       (False, _) -> childSY
  adjustedXY <- toLowLevel $ V2 (fromIntegral adjustedX) (fromIntegral adjustedY)

  adjustedXY'@(V2 x' y') <- fromLowLevel adjustedXY :: IO (V2 Float)
  case ((adjustedX + childWidth > parentWidth), (adjustedY + childHeight > parentHeight)) of
    (True, False) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (adjustedX + childWidth, parentHeight)))
    (False, True) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (parentWidth, adjustedY + childHeight)))
    (True, True) -> do
      atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (adjustedX + childWidth, adjustedY + childHeight)))
    _ -> return ()

  return adjustedXY

getParentGSVS :: GodotSimulaServer -> GodotWlrXWaylandSurface -> IO (Maybe GodotSimulaViewSprite)
getParentGSVS gss wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getParentGSVS" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getParentGSVS"
  simulaViewMap <- readTVarIO (gss ^. gssViews)
  parentWlrXWaylandSurface' <- G.get_parent wlrXWaylandSurface :: IO GodotWlrXWaylandSurface
  case validateObject parentWlrXWaylandSurface' of
    Nothing -> return Nothing
    Just validParentWlrXWaylandSurface -> do
      parentWlrXWaylandSurface <- validateSurfaceE validParentWlrXWaylandSurface
      let simulaViews = M.keys simulaViewMap
      let maybeParentSimulaView = Data.List.find (\simulaView -> (simulaView ^. svWlrEitherSurface) == (Right parentWlrXWaylandSurface)) simulaViews
      maybeGSVSParent <- case maybeParentSimulaView of
                              Nothing -> do
                                putStrLn "Couldn't find parent!"
                                return Nothing
                              (Just gsvsSimulaView) -> do let gsvsParent = Data.Maybe.fromJust (M.lookup gsvsSimulaView simulaViewMap)
                                                          return (Just gsvsParent)
      return maybeGSVSParent

markLikelyVisibleGSVSForFullRedraws :: GodotSimulaServer -> IO ()
markLikelyVisibleGSVSForFullRedraws gss = profileScope "Plugin.SimulaViewSprite.markLikelyVisibleGSVSForFullRedraws" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.markLikelyVisibleGSVSForFullRedraws"
  maybeKeyboardFocusedGSVS <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  mapM_ markGSVSForFullRedrawsByDefaultFrameAmount $ nub $ catMaybes -- removes Nothing's and duplicates from the list
    [ maybeKeyboardFocusedGSVS
    , maybeActiveCursorGSVS
    ]

handle_map_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_map_child gsvsInvisible gvArgs@[wlrXWaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_map_child" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_map_child"
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  debugPrintXWaylandMapDetails "Plugin.SimulaViewSprite.handle_map_child" wlrXWaylandSurface
  gss <- readTVarIO (gsvsInvisible ^. gsvsServer)
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  safeToDestroyArgs <- case maybeParentGSVS of
    Nothing -> do
      -- Since this surface's parent isn't known, we route to `handle_map_free_child`
      putStrLn "handle_map_child: re-routing to handle_map_free_child"
      handle_map_free_child_impl gsvsInvisible wlrXWaylandSurface
    Just (parentGSVS) -> do -- adjustedXY <- getAdjustedXY parentGSVS wlrXWaylandSurface
                            -- G.set_xy wlrXWaylandSurface adjustedXY
                            shouldCenter <- xWaylandSurfaceShouldCenterOverParent wlrXWaylandSurface
                            simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
                            atomically $ writeTVar (simulaView ^. svMapped) True
                            if shouldCenter
                              then markGSVSForFullRedrawFrames parentGSVS 30
                              else markGSVSForFullRedrawsByDefaultFrameAmount parentGSVS
                            markGSVSForFullRedrawsByDefaultFrameAmount gsvsInvisible
                            return False
  when safeToDestroyArgs $ mapM_ Api.godot_variant_destroy gvArgs
  return ()
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_unmap.computeSurfaceLocalCoordinates" $ do
          debugPutStrLn "Plugin.SimulaViewSprite.computeSurfaceLocalCoordinates"
          localX <- G.get_surface_origin_x child
          localY <- G.get_surface_origin_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> do
                            wlrXdgSurface <- validateSurfaceE wlrXdgSurface
                            return Nothing
                          Right parent -> do
                            parent <- validateSurfaceE parent
                            globalX <- G.get_surface_origin_x parent
                            globalY <- G.get_surface_origin_y parent
                            return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

handle_set_parent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_set_parent gsvs gvArgs@[wlrXWaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_set_parent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_set_parent"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  freeChildrenMap <- readTVarIO (gss ^. gssFreeChildren)
  let maybeFreeChildOwnerGSVS = M.lookup wlrXWaylandSurface freeChildrenMap
  shouldCenter <- xWaylandSurfaceShouldCenterOverParent wlrXWaylandSurface

  markGSVSForFullRedrawsByDefaultFrameAmount gsvs
  forM_ maybeParentGSVS $ \parentGSVS ->
    if shouldCenter
      then markGSVSForFullRedrawFrames parentGSVS 30
      else markGSVSForFullRedrawsByDefaultFrameAmount parentGSVS
  forM_ maybeFreeChildOwnerGSVS $ \freeChildOwnerGSVS ->
    when (Just freeChildOwnerGSVS /= maybeParentGSVS) $
      markGSVSForFullRedrawsByDefaultFrameAmount freeChildOwnerGSVS

  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_xdg_set_parent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_xdg_set_parent gsvs gvArgs@[_wlrXdgToplevelVariant] = profileScope "Plugin.SimulaViewSprite.handle_xdg_set_parent" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_xdg_set_parent"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  case simulaView ^. svWlrEitherSurface of
    Right _ -> return ()
    Left childWlrXdgSurface -> do
      childWlrXdgSurface <- validateSurfaceE childWlrXdgSurface
      maybeXdgDeclaredParentGSVS <- getXdgParentGSVS gss childWlrXdgSurface
      attachedMap <- readTVarIO (gss ^. gssAttachedXdgChildren)
      let maybeSimulaTrackedParentGSVS = M.lookup childWlrXdgSurface attachedMap
      case (maybeSimulaTrackedParentGSVS, maybeXdgDeclaredParentGSVS) of
        -- If state didn't change, then at least redraw the parent gsvs
        (Just simulaTrackedParentGSVS, Just xdgDeclaredParentGSVS)
          | simulaTrackedParentGSVS == xdgDeclaredParentGSVS ->
              markGSVSForFullRedrawsByDefaultFrameAmount simulaTrackedParentGSVS
        -- If we're switching from an old to a new parent, then change state and trigger a hard redraw on the new parent
        (Just _, Just xdgDeclaredParentGSVS) -> do
          detachXdgChildFromParent gss childWlrXdgSurface
          attachXdgChildToParent gss xdgDeclaredParentGSVS childWlrXdgSurface
          markGSVSForFullRedrawFrames xdgDeclaredParentGSVS 30
        -- If we used to be attached to a gsvs but now we're parentless:
        (Just _, Nothing) -> do
          detachXdgChildFromParent gss childWlrXdgSurface
          markGSVSForFullRedrawsByDefaultFrameAmount gsvs
        -- If we weren't attached to any gsvs before (meaning the gsvs is a
        -- standalone node in the scene graph) but now want to attach to a new
        -- one, we have to remove ourselves from the scene graph.
        (Nothing, Just xdgDeclaredParentGSVS) -> do
          removeStandaloneGSVSFromSceneGraph gsvs
          attachXdgChildToParent gss xdgDeclaredParentGSVS childWlrXdgSurface
          markGSVSForFullRedrawFrames xdgDeclaredParentGSVS 30
        -- If we're not attached to any gsvs (so we're a standalone gsvs in the scene graph),
        -- and still have no parent, then just mark the gsvs for redraws.
        (Nothing, Nothing) ->
          markGSVSForFullRedrawsByDefaultFrameAmount gsvs
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

-- Used for gsvs which are actually standalone gsvs rendered in the scene graph
-- (as opposed to attached to another gsvs as a child)
removeStandaloneGSVSFromSceneGraph :: GodotSimulaViewSprite -> IO ()
removeStandaloneGSVSFromSceneGraph gsvs = profileScope "Plugin.SimulaViewSprite.removeStandaloneGSVSFromSceneGraph" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.removeStandaloneGSVSFromSceneGraph"
  G.set_process gsvs False
  G.set_process_input gsvs False
  isInSceneGraph <- G.is_inside_tree ((safeCast gsvs) :: GodotNode)
  when isInSceneGraph $ do
    parentNode <- G.get_parent ((safeCast gsvs) :: GodotNode) :: IO GodotNode
    case validateObject parentNode of
      Just validParentNode -> removeChild validParentNode gsvs
      Nothing -> return ()

-- If the gsvs is standalone, mark it for redraws. Otherwise, find its parent
-- gsvs and mark it for redraws.
markRootGSVSForFullRedraws :: GodotSimulaViewSprite -> IO ()
markRootGSVSForFullRedraws gsvs = profileScope "Plugin.SimulaViewSprite.markRootGSVSForFullRedraws" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.markRootGSVSForFullRedraws"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  case simulaView ^. svWlrEitherSurface of
    Left wlrXdgSurface -> do
      attachedMap <- readTVarIO (gss ^. gssAttachedXdgChildren)
      case M.lookup wlrXdgSurface attachedMap of
        Just parentGSVS -> markGSVSForFullRedrawsByDefaultFrameAmount parentGSVS
        Nothing -> markGSVSForFullRedrawsByDefaultFrameAmount gsvs
    Right _ -> markGSVSForFullRedrawsByDefaultFrameAmount gsvs

handle_unmap_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_child self gvArgs@[wlrXWaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_unmap_child" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_child"
  gss <- readTVarIO (self ^. gsvsServer)
  wlrXWaylandSurface <-
    (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> do
      putStrLn "handle_unmap_child: no parent GSVS found"
      markLikelyVisibleGSVSForFullRedraws gss
    Just parentGSVS -> markGSVSForFullRedrawsByDefaultFrameAmount parentGSVS
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self gvArgs
  mapM_ Api.godot_variant_destroy gvArgs

handle_unmap_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_free_child self gvArgs@[wlrXWaylandSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_unmap_free_child" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_free_child"
  gss <- readTVarIO (self ^. gsvsServer)
  wlrXWaylandSurface <-
    (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> do
      debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_free_child: no parent GSVS found"
      markLikelyVisibleGSVSForFullRedraws gss
    Just parentGSVS -> markGSVSForFullRedrawsByDefaultFrameAmount parentGSVS
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self gvArgs
  mapM_ Api.godot_variant_destroy gvArgs


handle_unmap_base :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_base self _ = profileScope "Plugin.SimulaViewSprite.handle_unmap_base" $ do
  -- Since this shared helper doesn't own the [GodotVariant] args, we don't clean them up here
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_base"
  gss <- readTVarIO (self ^. gsvsServer)
  simulaView <- atomically $ readTVar (self ^. gsvsView)

  keyboardGrabLetGo gss (GrabWindow self undefined)
  keyboardGrabLetGo gss (GrabWindows undefined)
  keyboardGrabLetGo gss (GrabWorkspaces undefined)

  -- We don't dispatch on the [GodotVariant] argument since we don't know if it's xwayland or xdg.
  -- Instead, we go through `simulaView ^. svWlrEitherSurface`
  case simulaView ^. svWlrEitherSurface of
    Left wlrXdgSurface -> do
      detachedParent <- detachXdgChildFromParent gss wlrXdgSurface
      case detachedParent of
        Just parentGSVS -> do
          markGSVSForFullRedrawFrames parentGSVS 30
          clearKeyboardFocusIfThisView gss simulaView
        Nothing -> clearKeyboardFocusIfThisView gss simulaView
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      G.reference wlrXWaylandSurface
      detachedParent <- detachXWaylandFreeChildFromParent gss wlrXWaylandSurface
      case detachedParent of
        Just _ -> return ()
        Nothing -> clearKeyboardFocusIfThisView gss simulaView

  G.set_process self False
  G.set_process_input self False
  atomically $ writeTVar (simulaView ^. svMapped) False
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  isInSceneGraph <- G.is_inside_tree ((safeCast self) :: GodotNode)
  when isInSceneGraph $ do
    parentNode <- G.get_parent ((safeCast self) :: GodotNode) :: IO GodotNode
    case validateObject parentNode of
      Just validParentNode -> removeChild validParentNode self
      Nothing -> return ()
  where
    clearKeyboardFocusIfThisView :: GodotSimulaServer -> SimulaView -> IO ()
    clearKeyboardFocusIfThisView gss simulaView = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_destroy.clearKeyboardFocusIfThisView" $ do
      maybeGSVSFocused <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
      case maybeGSVSFocused of
        Nothing -> return ()
        Just gsvsFocused -> do
          simulaViewFocused <- readTVarIO (gsvsFocused ^. gsvsView)
          when (simulaViewFocused == simulaView) $ do
            debugHudPush gsvsFocused "KEY clear focus for unmapped/destroyed view"
            atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) Nothing

-- The returned GodotWlrSurface will have a +1'ed reference count by the time this function returns it; caller is responsible for unreferencing it
getXWaylandSubsurfaceAndCoords :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> CanvasBaseCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getXWaylandSubsurfaceAndCoords gsvs wlrXWaylandSurface compositionCoords = profileScope "Plugin.SimulaViewSprite.getXWaylandSubsurfaceAndCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getXWaylandSubsurfaceAndCoords"
  fmap (\(wlrSurface, coords, _) -> (wlrSurface, coords)) <$>
    getXWaylandSubsurfaceAndCoordsWithKeyboardFocus gsvs wlrXWaylandSurface compositionCoords

-- The returned GodotWlrSurface will have a +1'ed reference count by the time this function returns it; caller is responsible for unreferencing it
getXWaylandSubsurfaceAndCoordsWithKeyboardFocus :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> CanvasBaseCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXWaylandSurface))
getXWaylandSubsurfaceAndCoordsWithKeyboardFocus gsvs wlrXWaylandSurface compositionCoords@(CanvasBaseCoordinates (RightCoordinate sx) (DownCoordinate sy)) = profileScope "Plugin.SimulaViewSprite.getXWaylandSubsurfaceAndCoordsWithKeyboardFocus" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getXWaylandSubsurfaceAndCoordsWithKeyboardFocus"
  (offsetX, offsetY) <- getGSVSSurfacesCoordinateOffsetFromOrigin gsvs
  let coords = CanvasBaseCoordinates (RightCoordinate (sx - fromIntegral offsetX)) (DownCoordinate (sy - fromIntegral offsetY))
      CanvasBaseCoordinates (RightCoordinate rootSurfaceX) (DownCoordinate rootSurfaceY) = coords -- rootSurfaceX/rootSurfaceY are relative to the wlrXWaylandSurface
  freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
  maybeFreeRet <- getFreeChildrenCoords wlrXWaylandSurface coords freeChildren -- Returns referenced surface if it succeeds
  case maybeFreeRet of
     Just (wlrSurface, SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y), focusOwnerXWaylandSurface) -> do
       G.set_activated focusOwnerXWaylandSurface True
       debugPrintXWaylandFreeChildKeyboardFocus gsvs focusOwnerXWaylandSurface wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y))
       return $ Just (wlrSurface, SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y), focusOwnerXWaylandSurface)
     Nothing -> do
       maybeMappedChildRet <- getMappedXWaylandChildrenCoords coords wlrXWaylandSurface
       case maybeMappedChildRet of
         Just (wlrSurface, SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y), focusOwnerXWaylandSurface) -> do
           G.set_activated focusOwnerXWaylandSurface True
           debugPrintXWaylandFreeChildKeyboardFocus gsvs focusOwnerXWaylandSurface wlrSurface (SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y))
           return $ Just (wlrSurface, SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y), focusOwnerXWaylandSurface)
         Nothing -> do
           safeSetActivated gsvs True -- G.set_activated godotWlrXWaylandSurface True
           -- surface_at expects coordinates relative to the top level wlr_surface, so you don't need to worry about geometry rect offsets or anything
           withGodotRef (G.surface_at wlrXWaylandSurface rootSurfaceX rootSurfaceY :: IO GodotWlrSurfaceAtResult) $ \wlrSurfaceAtResult -> do
             subX <- G.get_sub_x wlrSurfaceAtResult
             subY <- G.get_sub_y wlrSurfaceAtResult
             withGodotRef (G.get_surface wlrSurfaceAtResult :: IO GodotWlrSurface) $ \wlrSurface' ->
               case validateObject wlrSurface' of
                 Nothing -> return Nothing
                 Just validWlrSurface -> do
                   validWlrSurface <- validateSurfaceE validWlrSurface
                   G.reference validWlrSurface -- Ensures the wlrSurface' is live when we return it from this function; caller is responsible for unreferencing it
                   return $ Just (validWlrSurface, SubSurfaceLocalCoordinates (RightCoordinate subX) (DownCoordinate subY), wlrXWaylandSurface)

-- Take gsvs coords and a mapped child and check whether or not the mapped child is hit or not. If so, return the hit in subsurface local coords.
-- Returns referenced surface if it succeeds; caller is responsible for unreferencing it
getHitMappedXWaylandChildWithCoords :: CanvasBaseCoordinates -> GodotWlrXWaylandSurface -> GodotWlrXWaylandSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXWaylandSurface))
getHitMappedXWaylandChildWithCoords (CanvasBaseCoordinates (RightCoordinate cx) (DownCoordinate cy)) parentWlrXWaylandSurface mappedChildXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getHitMappedXWaylandChildWithCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getHitMappedXWaylandChildWithCoords"
  (childRootX, childRootY) <- getEffectiveXWaylandChildSurfaceCoordsRelativeToParent parentWlrXWaylandSurface mappedChildXWaylandSurface
  let rootX = fromIntegral childRootX
      rootY = fromIntegral childRootY
      childLocalX = cx - rootX
      childLocalY = cy - rootY
  withGodotRef (G.surface_at mappedChildXWaylandSurface childLocalX childLocalY :: IO GodotWlrSurfaceAtResult) $ \wlrSurfaceAtResult -> do
    withGodotRef (G.get_surface wlrSurfaceAtResult :: IO GodotWlrSurface) $ \wlrSurface ->
      case validateObject wlrSurface of
        Nothing -> return Nothing
        Just validWlrSurface -> do
          validWlrSurface <- validateSurfaceE validWlrSurface
          (bufferWidth, bufferHeight) <- getBufferDimensions validWlrSurface
          subX <- G.get_sub_x wlrSurfaceAtResult
          subY <- G.get_sub_y wlrSurfaceAtResult
          case (0 <= subX, subX < fromIntegral bufferWidth, 0 <= subY, subY < fromIntegral bufferHeight) of
            (True, True, True, True) -> do
              G.reference validWlrSurface
              return $ Just (validWlrSurface, SubSurfaceLocalCoordinates (RightCoordinate subX) (DownCoordinate subY), mappedChildXWaylandSurface)
            _ -> return Nothing

-- Takes gsvs coords and checks whether each mapped child surface is hit or not (in reverse order). Does NOT check whether the root wlrXWaylandSurface is hit.
-- Returns referenced surface if it succeeds; caller is responsible for unreferencing it
getMappedXWaylandChildrenCoords :: CanvasBaseCoordinates -> GodotWlrXWaylandSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXWaylandSurface))
getMappedXWaylandChildrenCoords coords wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getMappedXWaylandChildrenCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getMappedXWaylandChildrenCoords"
  arrayOfChildren <- G.get_children wlrXWaylandSurface :: IO GodotArray
  arrayOfChildrenGV <- fromGodotArray arrayOfChildren
  children <- mapM fromGodotVariant arrayOfChildrenGV :: IO [GodotWlrXWaylandSurface]
  ret <- findMappedChildHit (Data.List.reverse children)
  Api.godot_array_destroy arrayOfChildren
  mapM_ Api.godot_variant_destroy arrayOfChildrenGV
  return ret
  where
    findMappedChildHit [] = return Nothing
    findMappedChildHit (child:remainingChildren) = do
      maybeNestedHit <- getMappedXWaylandChildrenCoords coords child
      case maybeNestedHit of
        Just hit -> return $ Just hit
        Nothing -> do
          maybeChildHit <- getHitMappedXWaylandChildWithCoords coords wlrXWaylandSurface child
          case maybeChildHit of
            Just hit -> return $ Just hit
            Nothing -> findMappedChildHit remainingChildren

-- Returns referenced surface if it succeeds; caller is responsible for unreferencing it
getFreeChildCoords :: GodotWlrXWaylandSurface -> CanvasBaseCoordinates -> GodotWlrXWaylandSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXWaylandSurface))
getFreeChildCoords parentWlrXWaylandSurface (CanvasBaseCoordinates (RightCoordinate cx) (DownCoordinate cy)) wlrXWaylandSurface = profileScope "Plugin.SimulaViewSprite.getFreeChildCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getFreeChildCoords"
  withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \freeChildSurface -> do
    fsx' <- G.get_surface_origin_x wlrXWaylandSurface
    fsy' <- G.get_surface_origin_y wlrXWaylandSurface

    let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
    (fLengthX', fLengthY') <- getBufferDimensions freeChildSurface
    let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
    case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
      (True, True, True, True) -> do
        G.reference freeChildSurface
        let x = cx - fsx
        let y = cy - fsy
        return $ Just (freeChildSurface, SubSurfaceLocalCoordinates (RightCoordinate x) (DownCoordinate y), wlrXWaylandSurface)
      _ -> return Nothing

getFreeChildrenCoords :: GodotWlrXWaylandSurface -> CanvasBaseCoordinates -> [GodotWlrXWaylandSurface] -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, GodotWlrXWaylandSurface))
getFreeChildrenCoords parentWlrXWaylandSurface coords@(CanvasBaseCoordinates (RightCoordinate cx) (DownCoordinate cy)) freeChildren = profileScope "Plugin.SimulaViewSprite.getFreeChildrenCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getFreeChildrenCoords"
  let freeChildren' = Data.List.reverse freeChildren
  case (freeChildren' == []) of
    True -> return Nothing
    False -> do let freeChildHead = Data.List.head freeChildren'
                maybeFreeChildCoords <- getFreeChildCoords parentWlrXWaylandSurface coords freeChildHead
                case maybeFreeChildCoords of
                     Nothing -> getFreeChildrenCoords parentWlrXWaylandSurface coords (Data.List.tail $ Data.List.reverse $ freeChildren')
                     Just ret -> return $ Just ret

getWlrSurfaceCoords :: CanvasBaseCoordinates -> (GodotWlrSurface, Int, Int)-> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getWlrSurfaceCoords cursorCoords@(CanvasBaseCoordinates (RightCoordinate cx) (DownCoordinate cy)) (wlrSurfaceFree, fsx', fsy') = profileScope "Plugin.SimulaViewSprite.getWlrSurfaceCoords" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.getWlrSurfaceCoords"
  (fLengthX', fLengthY') <- getBufferDimensions wlrSurfaceFree
  let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
  let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
  case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
    (True, True, True, True) -> do
      let ssx = cx - fsx
      let ssy = cy - fsy
      return $ Just (wlrSurfaceFree, SubSurfaceLocalCoordinates (RightCoordinate ssx) (DownCoordinate ssy))
    _ -> do return Nothing

handle_new_popup :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_new_popup gsvs gvArgs@[_wlrXdgSurfaceParentVariant] = profileScope "Plugin.SimulaViewSprite.handle_new_popup" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_new_popup"
  debugPrintCurrentMappedSurface "Plugin.SimulaViewSprite.handle_new_popup" gsvs
  markRootGSVSForFullRedraws gsvs
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_window_menu :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_window_menu gsvsInvisible gvArgs@[wlrXdgToplevel, serial, x, y] = profileScope "Plugin.SimulaViewSprite.handle_window_menu" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_window_menu"
  atomically $ writeTVar (gsvsInvisible ^. gsvsIsDamaged) True
  markRootGSVSForFullRedraws gsvsInvisible
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_new_subsurface :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_new_subsurface gsvs gvArgs@[wlrSubsurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface"
  wlrSubsurface <- (fromGodotVariant wlrSubsurfaceVariant :: IO GodotWlrSubsurface) >>= validateSurfaceE
  withGodotRef (G.get_wlr_surface wlrSubsurface :: IO GodotWlrSurface) $ \wlrSurface ->
    debugPrintWlrSurfaceMapDetails "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface.child" wlrSurface
  withGodotRef (G.get_wlr_surface_parent wlrSubsurface :: IO GodotWlrSurface) $ \wlrSurfaceParent ->
    debugPrintWlrSurfaceMapDetails "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface.parent" wlrSurfaceParent
  connectGodotSignal wlrSubsurface "destroy" gsvs "handle_wlr_subsurface_destroy" []
  markRootGSVSForFullRedraws gsvs
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

  -- Double-connecting?
  -- wlrSurface <- G.get_wlr_surface wlrSubsurface
  -- connectGodotSignal wlrSurface "new_subsurface" gsvs "handle_wlr_subsurface_new_subsurface" []
  -- connectGodotSignal wlrSurface "commit" gsvs "handle_wlr_subsurface_commit" []
  -- connectGodotSignal wlrSurface "destroy" gsvs "handle_wlr_subsurface_destroy" []
  -- wlrSurface <- G.get_wlr_surface wlrSubsurface
  -- connectGodotSignal wlrSurface "destroy" gsvs "handle_wlr_surface_destroy" []
  -- connectGodotSignal wlrSurface "commit" gsvs "handle_wlr_surface_commit" []

handle_wlr_subsurface_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_subsurface_destroy gsvs gvArgs@[_wlrSubsurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_wlr_subsurface_destroy" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_subsurface_destroy"
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  markRootGSVSForFullRedraws gsvs
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_commit :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_commit gsvs gvArgs@[_wlrSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_commit" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_commit"
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_destroy gsvs gvArgs@[_wlrSurfaceVariant] = profileScope "Plugin.SimulaViewSprite.handle_wlr_surface_destroy" $ do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_destroy"
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()
