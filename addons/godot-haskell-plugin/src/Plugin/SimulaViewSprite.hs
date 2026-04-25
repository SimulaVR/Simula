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
                      <*> atomically (newTVar 1.0)
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar (Nothing, Nothing))
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar 0)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar False)
                      <*> atomically (newTVar 0)
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar False)
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

-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
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
      atomically $ writeTVar (_gsvsShouldMove gsvs) False
  where -- Necessary for window manipulation to function
        setBoxShapeExtentsToMatchAABB :: GodotSimulaViewSprite -> IO ()
        setBoxShapeExtentsToMatchAABB gsvs = do
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
spriteReadyToMove gsvs = do
  debugPutStrLn "Plugin.SimulaViewSprite.spriteReadyToMove"
  shouldMove <- atomically $ readTVar (_gsvsShouldMove gsvs)
  if shouldMove then do meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
                        aabb <- G.get_aabb meshInstance
                        size <- godot_aabb_get_size aabb
                        vsize <- fromLowLevel size
                        return (vsize > 0) -- The first frame or so, the sprite has vsize 0
                   else return False

moveToStartingPosition :: GodotSimulaViewSprite -> String -> IO ()
moveToStartingPosition gsvs appLocation = do
  debugPutStrLn "Plugin.SimulaViewSprite.moveToStartingPosition"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  meshInstance <- atomically $ readTVar (gsvs ^. gsvsMeshInstance)
  aabb   <- G.get_aabb meshInstance
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
  let sizeY  = size ^. _y
  gsvsTransform <- G.get_global_transform gsvs
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
setTargetDimensions gsvs = do
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
  spilloverDims' <- toLowLevel $ (V2 (fromIntegral spilloverWidth) (fromIntegral spilloverHeight))
  V2 currentViewportWidth currentViewportHeight <- G.get_size renderTargetSurface >>= fromLowLevel :: IO (V2 Float)
  let viewportDimsChanged =
        ((round currentViewportWidth) /= spilloverWidth) || ((round currentViewportHeight) /= spilloverHeight)

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
  G.set_size renderTargetBase spilloverDims'
  G.set_size renderTargetSurface spilloverDims'
  when viewportDimsChanged $
    markGSVSForFullRedraws gsvs
  -- QuadMesh need to be scaled down by a factor of 0.001 to be reasonably sized in Godot:
  withQuadMesh gsvs $ \quadMesh ->
    G.set_size quadMesh =<< (toLowLevel $ (V2 (0.001 * (fromIntegral spilloverWidth)) (0.001 * (fromIntegral spilloverHeight))))

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
  case maybeSpilloverDimsOld of
    Nothing -> do
      if (spilloverWidth > 0) then atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just settledDimensions) else return ()
    Just spilloverDimsOld@(oldSpilloverWidth, oldSpilloverHeight) -> do
      case ((oldSpilloverWidth /= spilloverWidth) || (oldSpilloverHeight /= spilloverHeight), resizedLastFrame) of
        (False, _) -> return ()
        (True, True) -> do atomically $ do writeTVar (gsvs ^. gsvsResizedLastFrame) False
                                           writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                           when (transOld == 1) $
                             markGSVSForFullRedraws gsvs
        (True, False) -> do let pushX = spilloverWidth - oldSpilloverWidth
                            let pushY = spilloverHeight - oldSpilloverHeight
                            pushBackVector <- toLowLevel (V3 (-0.5 * 0.001 * (fromIntegral pushX)) (-0.5 * 0.001 * (fromIntegral pushY)) 0) :: IO GodotVector3
                            G.translate_object_local gsvs pushBackVector
                            atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                            case (transOld == 1, (spilloverWidth > targetWidth || spilloverHeight > targetHeight)) of
                              (True, False) ->  return () -- Avoid changing shader when apps first launch
                              (True, True) -> markGSVSForFullRedraws gsvs
                              (False, _) -> return ()
  updateQuadShader gsvs targetDims spilloverDims
  return ()

updateQuadShader :: GodotSimulaViewSprite -> SpriteDimensions -> (Int, Int) -> IO ()
updateQuadShader gsvs (SpriteDimensions (targetWidth, targetHeight)) (spilloverWidth, spilloverHeight) = do
  gsvsTransparency <- readTVarIO (gsvs ^. gsvsTransparency)
  fullRedrawFramesRemaining <- readTVarIO (gsvs ^. gsvsFullRedrawFramesRemaining)
  surfaceNeedsAlphaBlend <- surfaceHasInsetGeometry gsvs
  let shouldUseTransparentShader =
        (gsvsTransparency < 1)
        || (fullRedrawFramesRemaining > 0)
        || (spilloverWidth > targetWidth)
        || (spilloverHeight > targetHeight)
        || surfaceNeedsAlphaBlend
  setShader gsvs $
    if shouldUseTransparentShader
      then "res://addons/godot-haskell-plugin/TextShader.tres"
      else "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"

ready :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
ready self gvArgs = do
  debugPutStrLn "Plugin.SimulaViewSprite.ready"
  -- putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

inputEvent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
inputEvent self gvArgs@[_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] = do
  debugPutStrLn "Plugin.SimulaViewSprite.inputEvent"
  ev <- fromGodotVariant evObj
  clickPos <- fromGodotVariant clickPosObj
  processInputEvent self ev clickPos
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gsvs ev clickPos = do
  debugPutStrLn "Plugin.SimulaViewSprite.processInputEvent"
  -- putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processPointerMotionEvent gsvs clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processPointerButtonEvent gsvs (Button pressed button)
    return ()

processPointerMotionEvent :: GodotSimulaViewSprite -> GodotVector3 -> IO ()
processPointerMotionEvent gsvs clickPos = do
  debugPutStrLn "Plugin.SimulaViewSprite.processPointerMotionEvent"
  surfaceLocalCoords <- getSurfaceLocalCoordinates gsvs clickPos
  debugPrintPointerSeatState gsvs Motion surfaceLocalCoords
  processClickEvent' gsvs Motion surfaceLocalCoords

processPointerButtonEvent :: GodotSimulaViewSprite -> InputEventType -> IO ()
processPointerButtonEvent gsvs evt = do
  debugPutStrLn "Plugin.SimulaViewSprite.processPointerButtonEvent"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
  surfaceLocalCoords <- readTVarIO (gsvs ^. gsvsCursorCoordinates)
  debugPrintPointerSeatState gsvs evt surfaceLocalCoords
  maybeSerial <- notifyMouseButtonWithMaybeSerial wlrSeat evt
  when debugMouseEventsEnabled $
    mapM_
      (\serial ->
        putStrLn $
          "Mouse button delivery serial="
            ++ show serial)
      maybeSerial
  pointerNotifyFrame wlrSeat

newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = do
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
  atomically $ writeTVar (_gsvsCursorCoordinates gsvs) (SurfaceLocalCoordinates (0,0))

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
replaceActiveSurface gsvs maybeWlrSurface = do
  maybePreviousSurface <- atomically $ do
    previousSurface <- readTVar (gsvs ^. gsvsActiveSurface)
    writeTVar (gsvs ^. gsvsActiveSurface) maybeWlrSurface
    return previousSurface
  mapM_ (destroyMaybe . safeCast) maybePreviousSurface

clearActiveSurface :: GodotSimulaViewSprite -> IO ()
clearActiveSurface gsvs =
  replaceActiveSurface gsvs Nothing

replaceCursorSurface :: GodotSimulaViewSprite -> Maybe GodotWlrSurface -> IO ()
replaceCursorSurface gsvs maybeWlrSurface = do
  maybePreviousSurface <- atomically $ do
    (previousSurface, maybeCursorTexture) <- readTVar (gsvs ^. gsvsCursor)
    writeTVar (gsvs ^. gsvsCursor) (maybeWlrSurface, maybeCursorTexture)
    return previousSurface
  mapM_ (destroyMaybe . safeCast) maybePreviousSurface

clearCursorSurface :: GodotSimulaViewSprite -> IO ()
clearCursorSurface gsvs =
  replaceCursorSurface gsvs Nothing

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
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
        G.keyboard_notify_enter wlrSeat wlrSurface
        pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
        pointerNotifyFrame wlrSeat
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
        wlrSurface <- validateSurfaceE wlrSurface
        G.reference wlrSurface
        replaceActiveSurface gsvs (Just wlrSurface)
        safeSetActivated gsvs True -- G.set_activated wlrXWaylandSurface True
        G.keyboard_notify_enter wlrSeat wlrSurface
        pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
        pointerNotifyFrame wlrSeat

-- | This function isn't called unless a surface is being pointed at (by VR
-- | controllers or a mouse in pancake mode).
processClickEvent :: GodotSimulaViewSprite
                  -> InputEventType
                  -> GodotVector3
                  -> IO ()
processClickEvent gsvs evt clickPos = do
  surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) <- getSurfaceLocalCoordinates gsvs clickPos
  processClickEvent' gsvs evt surfaceLocalCoords

processClickEvent' :: GodotSimulaViewSprite
                  -> InputEventType
                  -> SurfaceLocalCoordinates
                  -> IO ()
processClickEvent' gsvs evt surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) = do
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  case isMapped of
    False -> putStrLn "processClickevent' isMapped is false!"
    True -> do let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
               maybeSurfaceAndCoords <-
                 case wlrEitherSurface of
                   Right godotWlrXWaylandSurface -> do
                     getXWaylandSubsurfaceAndCoords gsvs godotWlrXWaylandSurface surfaceLocalCoords
                   Left godotWlrXdgSurface ->
                     getXdgSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords
               case maybeSurfaceAndCoords of
                 Nothing -> do
                   wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
                   clearActiveSurface gsvs
                   G.pointer_clear_focus wlrSeat
                   case evt of
                     Motion -> return ()
                     Button _ _ -> pointerNotifyButton wlrSeat evt
                   pointerNotifyFrame wlrSeat
                 Just (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) ->
                   (do
                     wlrSeat <- readTVarIO (gss ^. gssWlrSeat)

                     case evt of
                       Motion -> do
                         pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                         pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                       Button _ _ -> do
                         pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                         pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                         debugPrintMouseButtonIntercept evt godotWlrSurface surfaceLocalCoords subSurfaceLocalCoords
                         pointerNotifyButton wlrSeat evt

                     pointerNotifyFrame wlrSeat)
                     `finally` destroyMaybe (safeCast godotWlrSurface)


debugPrintMouseButtonIntercept :: InputEventType -> GodotWlrSurface -> SurfaceLocalCoordinates -> SubSurfaceLocalCoordinates -> IO ()
debugPrintMouseButtonIntercept inputEventType wlrSurface (SurfaceLocalCoordinates (sx, sy)) (SubSurfaceLocalCoordinates (ssx, ssy)) =
  when debugMouseEventsEnabled $ do
    case inputEventType of
      Motion -> return ()
      Button pressed buttonIndex -> do
        (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
        putStrLn $
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

debugPrintPointerSeatState :: GodotSimulaViewSprite -> InputEventType -> SurfaceLocalCoordinates -> IO ()
debugPrintPointerSeatState gsvs inputEventType surfaceLocalCoords@(SurfaceLocalCoordinates (sx, sy)) =
  when debugMouseEventsEnabled $ do
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
        putStrLn $
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
  where
    getFocusedSurfaceSummary :: GodotWlrSeat -> IO String
    getFocusedSurfaceSummary wlrSeat =
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

    getHitSurfaceSummary :: GodotSimulaViewSprite -> SurfaceLocalCoordinates -> IO String
    getHitSurfaceSummary gsvs' coords = do
      simulaView <- readTVarIO (gsvs' ^. gsvsView)
      let wlrEitherSurface = simulaView ^. svWlrEitherSurface
      maybeSurfaceAndCoords <-
        case wlrEitherSurface of
          Right godotWlrXWaylandSurface ->
            getXWaylandSubsurfaceAndCoords gsvs' godotWlrXWaylandSurface coords
          Left godotWlrXdgSurface ->
            getXdgSubsurfaceAndCoords godotWlrXdgSurface coords
      case maybeSurfaceAndCoords of
        Nothing -> return "nothing"
        Just (godotWlrSurface, SubSurfaceLocalCoordinates (ssx, ssy)) ->
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

debugPrintWlrSurfaceMapDetails :: String -> GodotWlrSurface -> IO ()
debugPrintWlrSurfaceMapDetails prefix wlrSurface =
  when debugSurfaceCreationsEnabled $ do
    (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
    putStrLn $
      prefix
        ++ " surface="
        ++ show wlrSurface
        ++ " buffer="
        ++ show bufferWidth
        ++ "x"
        ++ show bufferHeight

debugPrintXWaylandMapDetails :: String -> GodotWlrXWaylandSurface -> IO ()
debugPrintXWaylandMapDetails prefix wlrXWaylandSurface =
  when debugSurfaceCreationsEnabled $ do
    x <- G.get_x wlrXWaylandSurface
    y <- G.get_y wlrXWaylandSurface
    width <- G.get_width wlrXWaylandSurface
    height <- G.get_height wlrXWaylandSurface
    pid <- G.get_pid wlrXWaylandSurface
    putStrLn $
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

debugPrintXdgMapDetails :: String -> GodotWlrXdgSurface -> IO ()
debugPrintXdgMapDetails prefix wlrXdgSurface =
  when debugSurfaceCreationsEnabled $ do
    roleInt <- G.get_role wlrXdgSurface
    pid <- G.get_pid wlrXdgSurface
    V2 (V2 posX posY) (V2 width height) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
    withGodotRef (G.get_wlr_surface wlrXdgSurface :: IO GodotWlrSurface) $ \wlrSurface -> do
      (bufferWidth, bufferHeight) <- getBufferDimensions wlrSurface
      putStrLn $
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
 where
  xdgRoleName 0 = "none"
  xdgRoleName 1 = "toplevel"
  xdgRoleName 2 = "popup"
  xdgRoleName role = "unknown(" ++ show role ++ ")"

debugPrintCurrentMappedSurface :: String -> GodotSimulaViewSprite -> IO ()
debugPrintCurrentMappedSurface prefix gsvs =
  when debugSurfaceCreationsEnabled $ do
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
getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getXdgSubsurfaceAndCoords wlrXdgSurface cursorCoords@(SurfaceLocalCoordinates (sx, sy)) = do
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
          let ssCoordinates = SubSurfaceLocalCoordinates (ssx, ssy)
          return $ Just (validWlrSurfaceSubSurface, ssCoordinates)

keyboardNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> IO ()
keyboardNotifyEnter wlrSeat wlrSurface = do
  debugPutStrLn "Plugin.SimulaViewSprite.keyboardNotifyEnter"
  G.keyboard_notify_enter wlrSeat wlrSurface

-- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
-- | need one since the GodotWlrSeat keeps internal track of what the currently
-- | active surface is.
pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyMotion"
  G.pointer_notify_motion wlrSeat ssx ssy
  -- putStrLn $ "G.point_notify_motion: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
pointerNotifyButton wlrSeat inputEventType = do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyButton"
  notifyMouseButtonWithMaybeSerial wlrSeat inputEventType >> return ()

notifyMouseButtonWithMaybeSerial :: GodotWlrSeat -> InputEventType -> IO (Maybe Int)
notifyMouseButtonWithMaybeSerial wlrSeat inputEventType =
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
pointerNotifyFrame wlrSeat = do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyFrame"
  G.pointer_notify_frame wlrSeat

-- | Let wlroots know we have entered a new surface. We can safely call this
-- | over and over (wlroots checks if we've called it already for this surface
-- | and, if so, returns early.
pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  debugPutStrLn "Plugin.SimulaViewSprite.pointerNotifyEnter"
  G.pointer_notify_enter wlrSeat wlrSurface ssx ssy -- Causing a crash

_handle_map :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_map gsvs _ = do
  debugPutStrLn "Plugin.SimulaViewSprite._handle_map"
  debugPrintCurrentMappedSurface "Plugin.SimulaViewSprite._handle_map" gsvs
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    Left wlrXdgSurface -> do
      validateSurfaceE wlrXdgSurface
      -- putStrLn $ "Refraining from setting wlrXdgSurface position"
      return ()
    Right wlrXWaylandSurface -> do -- Safeguard to prevent potentially weird behavior
                                   validateSurfaceE wlrXWaylandSurface
                                   zero <- toLowLevel (V2 0 0)
                                   G.set_xy wlrXWaylandSurface zero
                                   return ()

  G.set_process gsvs True
  G.set_process_input gsvs True
  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)
  atomically $ writeTVar (_gsvsShouldMove gsvs) True

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
    else markGSVSForFullRedraws gsvs

  setInFrontOfUser gsvs (-2)

  V3 1 1 1 ^* (1 + 1 * 1) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)

  focus gsvs -- We're relying on this to add references to wlrSurface :/

  atomically $ writeTVar (simulaView ^. svMapped) True

  return ()

surfaceHasParent :: Either GodotWlrXdgSurface GodotWlrXWaylandSurface -> IO Bool
surfaceHasParent eitherSurface =
  case eitherSurface of
    Left wlrXdgSurface -> do
      wlrXdgSurface <- validateSurfaceE wlrXdgSurface
      toplevel <- G.get_xdg_toplevel wlrXdgSurface >>= validateSurfaceE
      parent <- G.get_parent toplevel :: IO GodotWlrXdgToplevel
      return $ isJust (validateObject parent)
    Right wlrXWaylandSurface -> do
      wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
      parent <- G.get_parent wlrXWaylandSurface :: IO GodotWlrXWaylandSurface
      return $ isJust (validateObject parent)

handle_unmap :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap self args@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap"
  handle_unmap_base self args

-- Passes control entirely to updateSimulaViewSprite.
_process :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_process self gvArgs = do
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
        resizeToTargetDimsIfBigEnough gsvs = do
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
_handle_destroy gsvs gvArgs@[_gsvsGV] = do
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
moveSpriteAlongObjectZAxis gsvs dist = do
  debugPutStrLn "Plugin.SimulaViewSprite.moveSpriteAlongObjectZAxis"
  orientSpriteTowardsGaze gsvs
  pushBackVector <- toLowLevel (V3 0 0 dist) :: IO GodotVector3 -- For some reason we also have to shift the vector 0.5 units to the right
  G.translate_object_local gsvs pushBackVector
  return ()

setInFrontOfUser :: GodotSimulaViewSprite -> Float -> IO ()
setInFrontOfUser gsvs zAxisDist = do
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
safeSetActivated gsvs active = do
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
applyViewportBaseTexture gsvs = do
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
handle_map_free_child gsvsInvisible gvArgs@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_map_free_child"
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  debugPrintXWaylandMapDetails "Plugin.SimulaViewSprite.handle_map_free_child" wlrXWaylandSurface
  surfaceWasMappedAndReferenced <- handle_map_free_child_impl gsvsInvisible wlrXWaylandSurface
  when surfaceWasMappedAndReferenced $ mapM_ Api.godot_variant_destroy gvArgs
  return ()

-- Returns True if the surface gets `G.reference`'ed
handle_map_free_child_impl :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO Bool
handle_map_free_child_impl gsvsInvisible wlrXWaylandSurface = do
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
                                           x <- G.get_x wlrXWaylandSurface
                                           y <- G.get_y wlrXWaylandSurface
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
                                           markGSVSForFullRedraws gsvs
                                           return True

  simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
  atomically $ writeTVar (simulaView ^. svMapped) True
  return surfaceMappedAndReferenced
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = do
          debugPutStrLn "Plugin.SimulaViewSprite.computeSurfaceLocalCoordinates"
          localX <- G.get_x child
          localY <- G.get_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> do
                            validateSurfaceE wlrXdgSurface
                            return Nothing
                          Right parent -> do
                            validateSurfaceE parent
                            globalX <- G.get_x parent
                            globalY <- G.get_y parent
                            return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

getAdjustedXY :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO GodotVector2
getAdjustedXY gsvs wlrXWaylandSurface = do
  debugPutStrLn "Plugin.SimulaViewSprite.getAdjustedXY"
  childSX <- G.get_x wlrXWaylandSurface
  childSY <- G.get_y wlrXWaylandSurface
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
getAdjustedXYFreeChild gsvs wlrXWaylandSurface = do
  debugPutStrLn "Plugin.SimulaViewSprite.getAdjustedXYFreeChild"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  (V2 parentX parentY) <- case eitherSurface of
                               (Left wlrXdgSurface) -> do
                                 wlrXdgSurface <- validateSurfaceE wlrXdgSurface
                                 return (V2 0 0)
                               (Right wlrXWaylandSurfaceParent) -> do
                                 wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurfaceParent
                                 x <- G.get_x wlrXWaylandSurfaceParent
                                 y <- G.get_y wlrXWaylandSurfaceParent
                                 return $ V2 x y
  childX <- G.get_x wlrXWaylandSurface
  childY <- G.get_y wlrXWaylandSurface

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
getParentGSVS gss wlrXWaylandSurface = do
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

handle_map_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_map_child gsvsInvisible gvArgs@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_map_child"
  gss <- readTVarIO $ (gsvsInvisible ^. gsvsServer)
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
                            simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
                            atomically $ writeTVar (simulaView ^. svMapped) True
                            markGSVSForFullRedraws parentGSVS
                            markGSVSForFullRedraws gsvsInvisible
                            return False
  when safeToDestroyArgs $ mapM_ Api.godot_variant_destroy gvArgs
  return ()
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = do
          debugPutStrLn "Plugin.SimulaViewSprite.computeSurfaceLocalCoordinates"
          localX <- G.get_x child
          localY <- G.get_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> do
                            wlrXdgSurface <- validateSurfaceE wlrXdgSurface
                            return Nothing
                          Right parent -> do
                            parent <- validateSurfaceE parent
                            globalX <- G.get_x parent
                            globalY <- G.get_y parent
                            return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

handle_set_parent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_set_parent gsvs gvArgs@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_set_parent"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  freeChildrenMap <- readTVarIO (gss ^. gssFreeChildren)
  let maybeFreeChildOwnerGSVS = M.lookup wlrXWaylandSurface freeChildrenMap

  markGSVSForFullRedraws gsvs
  forM_ maybeParentGSVS markGSVSForFullRedraws
  forM_ maybeFreeChildOwnerGSVS $ \freeChildOwnerGSVS ->
    when (Just freeChildOwnerGSVS /= maybeParentGSVS) $
      markGSVSForFullRedraws freeChildOwnerGSVS

  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_unmap_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_child self gvArgs@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_child"
  gss <- readTVarIO (self ^. gsvsServer)
  wlrXWaylandSurface <-
    (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> putStrLn "handle_unmap_child: no parent GSVS found"
    Just parentGSVS -> markGSVSForFullRedraws parentGSVS
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self gvArgs
  mapM_ Api.godot_variant_destroy gvArgs

handle_unmap_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_free_child self gvArgs@[wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_free_child"
  gss <- readTVarIO (self ^. gsvsServer)
  wlrXWaylandSurface <-
    (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> putStrLn "handle_unmap_free_child: no parent GSVS found"
    Just parentGSVS -> markGSVSForFullRedraws parentGSVS
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self gvArgs
  mapM_ Api.godot_variant_destroy gvArgs


handle_unmap_base :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_base self [wlrXWaylandSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_unmap_base"
  gss <- readTVarIO (self ^. gsvsServer)
  simulaView <- atomically $ readTVar (self ^. gsvsView)
  freeChildrenMap <- readTVarIO (gss ^. gssFreeChildren)
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE

  keyboardGrabLetGo gss (GrabWindow self undefined)
  keyboardGrabLetGo gss (GrabWindows undefined)
  keyboardGrabLetGo gss (GrabWorkspaces undefined)

  G.reference wlrXWaylandSurface
  let maybeGSVSParent = M.lookup wlrXWaylandSurface freeChildrenMap

  case maybeGSVSParent of
    Just parentGSVS -> do -- Delete the free child from the parentGSVS and the gssFreeChildren map
                  -- putStrLn "handle_unmap_base: free child path"
                  freeChildren <- readTVarIO (parentGSVS ^. gsvsFreeChildren)
                  let freeChildrenNew = Data.List.delete wlrXWaylandSurface freeChildren
                  atomically $ writeTVar (parentGSVS ^. gsvsFreeChildren) freeChildrenNew
                  let freeChildrenMapNew = M.delete wlrXWaylandSurface freeChildrenMap
                  atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew


    Nothing -> do
                  -- putStrLn "handle_unmap_base: normal child path"
                  simulaView <- readTVarIO (self ^. gsvsView)
                  atomically $ writeTVar (simulaView ^. svMapped) False

                  -- Ensure that we de-focus the gsvs if it is active
                  maybeGSVSFocused <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
                  case maybeGSVSFocused of
                    Nothing -> return ()
                    (Just gsvsFocused) -> do
                      simulaViewFocused <- readTVarIO (gsvsFocused ^. gsvsView)
                      if (simulaViewFocused == simulaView) then (atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) Nothing)
                                                          else (return ())

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

-- The returned GodotWlrSurface will have a +1'ed reference count by the time this function returns it; caller is responsible for unreferencing it
getXWaylandSubsurfaceAndCoords :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getXWaylandSubsurfaceAndCoords gsvs wlrXWaylandSurface coords@(SurfaceLocalCoordinates (sx, sy)) = do
  debugPutStrLn "Plugin.SimulaViewSprite.getXWaylandSubsurfaceAndCoords"
  freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
  maybeFreeRet <- getFreeChildrenCoords wlrXWaylandSurface coords freeChildren -- Returns referenced surface if it succeeds
  case maybeFreeRet of
     Just freeSurfaceAndCoords@(wlrSurface, SubSurfaceLocalCoordinates (x, y), maybeWlrXWaylandSurface) -> do
       let wlrXWaylandSurface = Data.Maybe.fromJust maybeWlrXWaylandSurface
       G.set_activated wlrXWaylandSurface True
       return $ Just (wlrSurface, SubSurfaceLocalCoordinates (x, y))
     Nothing -> do
       safeSetActivated gsvs True -- G.set_activated godotWlrXWaylandSurface True
       -- surface_at expects coordinates relative to the top level wlr_surface, so you don't need to worry about geometry rect offsets or anything
       withGodotRef (G.surface_at wlrXWaylandSurface sx sy :: IO GodotWlrSurfaceAtResult) $ \wlrSurfaceAtResult -> do
         subX <- G.get_sub_x wlrSurfaceAtResult
         subY <- G.get_sub_y wlrSurfaceAtResult
         withGodotRef (G.get_surface wlrSurfaceAtResult :: IO GodotWlrSurface) $ \wlrSurface' ->
           case validateObject wlrSurface' of
             Nothing -> return Nothing
             Just validWlrSurface -> do
               validWlrSurface <- validateSurfaceE validWlrSurface
               G.reference validWlrSurface -- Ensures the wlrSurface' is live when we return it from this function; caller is responsible for unreferencing it
               return $ Just (validWlrSurface, SubSurfaceLocalCoordinates (subX, subY))

-- Returns referenced surface if it succeeds; caller is responsible for unreferencing it
getFreeChildCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> GodotWlrXWaylandSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildCoords parentWlrXWaylandSurface (SurfaceLocalCoordinates (cx, cy)) wlrXWaylandSurface = do
  debugPutStrLn "Plugin.SimulaViewSprite.getFreeChildCoords"
  withGodotRef (G.get_wlr_surface wlrXWaylandSurface :: IO GodotWlrSurface) $ \freeChildSurface -> do
    fsx' <- G.get_x wlrXWaylandSurface
    fsy' <- G.get_y wlrXWaylandSurface

    let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
    (fLengthX', fLengthY') <- getBufferDimensions freeChildSurface
    let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
    case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
      (True, True, True, True) -> do
        G.reference freeChildSurface
        let x = cx - fsx
        let y = cy - fsy
        return $ Just (freeChildSurface, SubSurfaceLocalCoordinates (x, y), Just wlrXWaylandSurface)
      _ -> return Nothing

getFreeChildrenCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> [GodotWlrXWaylandSurface] -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildrenCoords parentWlrXWaylandSurface coords@(SurfaceLocalCoordinates (cx, cy)) freeChildren = do
  debugPutStrLn "Plugin.SimulaViewSprite.getFreeChildrenCoords"
  let freeChildren' = Data.List.reverse freeChildren
  case (freeChildren' == []) of
    True -> return Nothing
    False -> do let freeChildHead = Data.List.head freeChildren'
                maybeFreeChildCoords <- getFreeChildCoords parentWlrXWaylandSurface coords freeChildHead
                case maybeFreeChildCoords of
                     Nothing -> getFreeChildrenCoords parentWlrXWaylandSurface coords (Data.List.tail $ Data.List.reverse $ freeChildren')
                     Just ret -> return $ Just ret

getWlrSurfaceCoords :: SurfaceLocalCoordinates -> (GodotWlrSurface, Int, Int)-> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates))
getWlrSurfaceCoords cursorCoords@(SurfaceLocalCoordinates (cx, cy)) (wlrSurfaceFree, fsx', fsy') = do
  debugPutStrLn "Plugin.SimulaViewSprite.getWlrSurfaceCoords"
  (fLengthX', fLengthY') <- getBufferDimensions wlrSurfaceFree
  let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
  let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
  case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
    (True, True, True, True) -> do
      let ssx = cx - fsx
      let ssy = cy - fsy
      return $ Just (wlrSurfaceFree, SubSurfaceLocalCoordinates (ssx, ssy))
    _ -> do return Nothing

handle_new_popup :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_new_popup gsvs gvArgs@[_wlrXdgSurfaceParentVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_new_popup"
  debugPrintCurrentMappedSurface "Plugin.SimulaViewSprite.handle_new_popup" gsvs
  markGSVSForFullRedraws gsvs
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_window_menu :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_window_menu gsvsInvisible gvArgs@[wlrXdgToplevel, serial, x, y] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_window_menu"
  atomically $ writeTVar (gsvsInvisible ^. gsvsIsDamaged) True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_new_subsurface :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_new_subsurface gsvs gvArgs@[wlrSubsurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface"
  wlrSubsurface <- (fromGodotVariant wlrSubsurfaceVariant :: IO GodotWlrSubsurface) >>= validateSurfaceE
  withGodotRef (G.get_wlr_surface wlrSubsurface :: IO GodotWlrSurface) $ \wlrSurface ->
    debugPrintWlrSurfaceMapDetails "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface.child" wlrSurface
  withGodotRef (G.get_wlr_surface_parent wlrSubsurface :: IO GodotWlrSurface) $ \wlrSurfaceParent ->
    debugPrintWlrSurfaceMapDetails "Plugin.SimulaViewSprite.handle_wlr_surface_new_subsurface.parent" wlrSurfaceParent
  connectGodotSignal wlrSubsurface "destroy" gsvs "handle_wlr_subsurface_destroy" []
  markGSVSForFullRedraws gsvs
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
handle_wlr_subsurface_destroy gsvs gvArgs@[_wlrSubsurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_subsurface_destroy"
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_commit :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_commit gsvs gvArgs@[_wlrSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_commit"
  mapM_ Api.godot_variant_destroy gvArgs
  return ()

handle_wlr_surface_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_destroy gsvs gvArgs@[_wlrSurfaceVariant] = do
  debugPutStrLn "Plugin.SimulaViewSprite.handle_wlr_surface_destroy"
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  mapM_ Api.godot_variant_destroy gvArgs
  return ()
