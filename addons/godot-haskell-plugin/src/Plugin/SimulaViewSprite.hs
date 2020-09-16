{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.SimulaViewSprite where

import Data.Text
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

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance NativeScript GodotSimulaViewSprite where
  className = "SimulaViewSprite"
  classInit obj =
    do putStrLn "SimulaViewSprite()"
       GodotSimulaViewSprite (safeCast obj)
                      <$> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (False, 0))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar (error "Failed to initialize GodotSimulaViewSprite."))
                      <*> atomically (newTVar Nothing)
                      <*> atomically (newTVar [])
                      -- <*> atomically (newTVar False)
  -- classExtends = "RigidBody"
  classMethods =
    [ func NoRPC "_input_event" inputEvent
    , func NoRPC "_ready" ready

    , func NoRPC "_handle_destroy" _handle_destroy
    , func NoRPC "_handle_map" _handle_map
    , func NoRPC "_process" Plugin.SimulaViewSprite._process
    , func NoRPC "handle_unmap" handle_unmap
    , func NoRPC "handle_unmap_child" handle_unmap_child
    , func NoRPC "handle_unmap_free_child" handle_unmap_free_child
    , func NoRPC "handle_map_free_child" handle_map_free_child
    , func NoRPC "handle_map_child" handle_map_child
    , func NoRPC "handle_set_parent" handle_set_parent
    ]

  -- Test:
  classSignals = [ signal "map" [("gsvs", GodotVariantTypeObject)]
                 , signal "map_free_child" [("wlrXWaylandSurface", GodotVariantTypeObject)]
                 ]

-- | Updates the GodotSimulaViewSprite state (including updating its texture).
-- | Intended to be called every frame.
updateSimulaViewSprite :: GodotSimulaViewSprite -> IO ()
updateSimulaViewSprite gsvs = do
  setTargetDimensions gsvs
  applyViewportBaseTexture gsvs
  setBoxShapeExtentsToMatchAABB gsvs

  (isStartingApp, startingPositionIndex) <- (readTVarIO (gsvs ^. gsvsShouldMove))
  case isStartingApp of
    False -> return ()
    True -> do
      whenM (spriteReadyToMove gsvs) $ do
          atomically $ writeTVar (_gsvsShouldMove gsvs) (False, 0)
          moveToStartingPosition gsvs startingPositionIndex

  -- sprite3D <- readTVarIO (gsvs ^. gsvsSprite)
  -- opacityFloat <- G.get_opacity sprite3D
  -- G.set_opacity sprite3D 0.5
  -- putStrLn $ "GSVS opacity: " ++ (show opacityFloat)


  where -- Necessary for window manipulation to function
        setBoxShapeExtentsToMatchAABB :: GodotSimulaViewSprite -> IO ()
        setBoxShapeExtentsToMatchAABB gsvs = do
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
  (isStartingApp, startingPositionIndex) <- atomically $ readTVar (_gsvsShouldMove gsvs)
  if isStartingApp then do meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
                           aabb <- G.get_aabb meshInstance
                           size <- godot_aabb_get_size aabb
                           vsize <- fromLowLevel size
                           return (vsize > 0) -- The first frame or so, the sprite has vsize 0
                   else return False

moveToStartingPosition :: GodotSimulaViewSprite -> Int -> IO ()
moveToStartingPosition gsvs appPositionIndex = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  meshInstance <- atomically $ readTVar (gsvs ^. gsvsMeshInstance)
  aabb   <- G.get_aabb meshInstance
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
  case ((appPositionIndex - 1), (appPositionIndex - 1) `mod` 4) of
    (0, _) -> do gsvsTransform <- G.get_global_transform gsvs
                 atomically $ writeTVar (gss ^. gssStartingAppTransform) (Just gsvsTransform)
                 moveSpriteAlongObjectZAxis gsvs 0.3
    (_, 1) -> do startingAppTransform <- readTVarIO (gss ^. gssStartingAppTransform)
                 case startingAppTransform of
                   Just transform -> G.set_global_transform gsvs transform
                   Nothing -> return ()
                 G.translate gsvs =<< toLowLevel (V3 (-sizeX) 0 0)
    (_, 2) -> do startingAppTransform <- readTVarIO (gss ^. gssStartingAppTransform)
                 case startingAppTransform of
                   Just transform -> G.set_global_transform gsvs transform
                   Nothing -> return ()
                 G.translate gsvs =<< toLowLevel (V3 0 (-sizeX) 0)
    (_, 3) -> do startingAppTransform <- readTVarIO (gss ^. gssStartingAppTransform)
                 case startingAppTransform of
                   Just transform -> G.set_global_transform gsvs transform
                   Nothing -> return ()
                 G.translate gsvs =<< toLowLevel (V3 (sizeX) 0 0)
    (_, 0) -> do startingAppTransform <- readTVarIO (gss ^. gssStartingAppTransform)
                 case startingAppTransform of
                   Just transform -> G.set_global_transform gsvs transform
                   Nothing -> return ()
                 G.translate gsvs =<< toLowLevel (V3 0 (sizeX) 0)
    _ -> return ()
  orientSpriteTowardsGaze gsvs

  -- Launch next starting app, if there is one
  gss <- readTVarIO (gsvs ^. gsvsServer)
  sApps <- readTVarIO (gss ^. gssStartingApps)
  let nextApp = if (sApps == []) then Nothing else Just (Data.List.head sApps)
  case nextApp of
    Nothing -> return ()
    Just app -> do let tailApps = (Data.List.tail sApps)
                   atomically $ writeTVar (gss ^. gssStartingApps) tailApps
                   let secondApp = if tailApps == [] then Nothing else Just (Data.List.head tailApps)
                   case secondApp of
                     Nothing -> focus gsvs
                     Just secondApp' -> appStrLaunch gss secondApp'

-- Sets gsvs wlr_xwayland_surface size and all associated viewports to the
-- gsvsTargetSize every frame
setTargetDimensions :: GodotSimulaViewSprite -> IO ()
setTargetDimensions gsvs = do
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  renderTargetBase <- readTVarIO (cb ^. cbViewport)
  surfaceMap <- readTVarIO (gsvs ^. gsvsSurfaceMap)
  let surfaces = MO.assocs surfaceMap
  let css = fmap snd surfaces
  csViewports <- mapM (\cs -> readTVarIO (cs ^. csViewport)) css
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurface <- getWlrSurface eitherSurface

  -- Get state
  originalDims@(originalWidth, originalHeight) <- getBufferDimensions wlrSurface
  maybeTargetDims <- readTVarIO (gsvs ^. gsvsTargetSize)
  targetDims@(SpriteDimensions (targetWidth, targetHeight)) <- case maybeTargetDims of
        Nothing -> do
          -- atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions originalDims))
          return (SpriteDimensions originalDims)
        Just targetDims' -> return targetDims'

  -- Try to avoid forcing small popups to be large squares.
  let settledDimensions@(settledWidth, settledHeight) = if (originalWidth > 450 || originalHeight > 450)
        then (targetWidth, targetHeight)
        else (originalWidth, originalHeight)

  settledDimensions' <- toLowLevel $ (V2 (fromIntegral settledWidth) (fromIntegral settledHeight))

  -- Set buffer dimensions to new target size
  case eitherSurface of
    Left wlrXdgSurface -> return () -- TODO: Fix xdg functionality
    Right wlrXWaylandSurface -> do G.set_size wlrXWaylandSurface settledDimensions'

  -- Set the corresponding Viewports to match our new target size
  G.set_size renderTargetBase settledDimensions'
  mapM (\renderTargetSurface -> G.set_size renderTargetSurface settledDimensions') csViewports
  quadMesh <- getQuadMesh gsvs
  G.set_size quadMesh =<< (toLowLevel $ (V2 (0.001 * (fromIntegral settledWidth)) (0.001 * (fromIntegral settledHeight)))) -- QuadMesh need to be significantly scaled down to be normally sized in Godot
  return ()

ready :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
ready self _ = do
  -- putStrLn "ready in SimulaViewSprite.hs"
  G.set_mode self RigidBody.MODE_KINEMATIC
  return ()

inputEvent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
inputEvent self [_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] = do
  ev <- fromGodotVariant evObj
  clickPos <- fromGodotVariant clickPosObj
  processInputEvent self ev clickPos
  godot_object_destroy ev
  return ()

-- Needs more descriptive type constructor
data InputEventType
  = Motion
  | Button Bool -- (GodotIsPressed Bool)       ?
           Int  -- (GodotButtonDescriptor Int) ?

-- | Handles mouse (i.e., non-VR controller) events in pancake mode.
processInputEvent :: GodotSimulaViewSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gsvs ev clickPos = do
  -- putStrLn "processInputEvent"
  whenM (ev `isClass` "InputEventMouseMotion") $ processClickEvent gsvs Motion clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processClickEvent gsvs (Button pressed button) clickPos
    return ()

newGodotSimulaViewSprite :: GodotSimulaServer -> SimulaView -> IO (GodotSimulaViewSprite)
newGodotSimulaViewSprite gss simulaView = do
  gsvsObj <- "res://addons/godot-haskell-plugin/SimulaViewSprite.gdns"
    & newNS' [] :: IO GodotObject
  maybeGSVS <- asNativeScript gsvsObj :: IO (Maybe GodotSimulaViewSprite)
  let gsvs = Data.Maybe.fromJust maybeGSVS


  meshInstance <- unsafeInstance GodotMeshInstance "MeshInstance"
  quadMesh <- unsafeInstance GodotQuadMesh "QuadMesh"
  G.set_mesh meshInstance (safeCast quadMesh)
  G.add_child gsvs (safeCast meshInstance) True

  shader <- load GodotShader "Shader" "res://addons/godot-haskell-plugin/TextShader.tres"
  case shader of
    Just shader -> do
      shm <- unsafeInstance GodotShaderMaterial "ShaderMaterial"
      G.set_shader shm shader
      G.set_material quadMesh (safeCast shm)
    Nothing -> error "couldn't fetch shader, hard failing for debug purposes"

  godotBoxShape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gsvs (safeCast gsvs)
  G.shape_owner_add_shape gsvs ownerId (safeCast godotBoxShape)

  atomically $ writeTVar (_gsvsServer            gsvs) gss
  atomically $ writeTVar (_gsvsMeshInstance      gsvs) meshInstance
  atomically $ writeTVar (_gsvsShape             gsvs) godotBoxShape
  atomically $ writeTVar (_gsvsView              gsvs) simulaView
  atomically $ writeTVar (_gsvsCursorCoordinates gsvs) (SurfaceLocalCoordinates (0,0))

  -- Set config settings
  keyboardShortcuts <- readTVarIO (gss ^. gssKeyboardShortcuts)
  configuration <- readTVarIO (gss ^. gssConfiguration)
  let windowScale = realToFrac (configuration ^. defaultWindowScale) :: Float
  (V3 1 1 1 ^* (windowScale)) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)

  let maybeWindowResolution = (configuration ^. defaultWindowResolution) :: Maybe (Dhall.Natural, Dhall.Natural)
  case maybeWindowResolution of
    Just windowResolution'@(x, y) -> do atomically $ writeTVar (gsvs ^. gsvsTargetSize) (Just (SpriteDimensions (fromIntegral x, fromIntegral y)))
    Nothing -> return () -- If we don't have a target size, we delay setting QuadMesh size until we're able to retrieve its buffer dimensions

  G.set_process gsvs False

  return gsvs

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView)
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)

  atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) (Just gsvs)
  atomically $ writeTVar (gss ^. gssActiveCursorGSVS) (Just gsvs)

  case wlrEitherSurface of
    Left wlrXdgSurface -> do wlrSurface  <- G.get_wlr_surface wlrXdgSurface
                             G.reference wlrSurface
                             toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
                             -- isGodotTypeNull wlrSurface
                             G.set_activated toplevel True
                             -- G.keyboard_notify_enter wlrSeat wlrSurface
                             pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
                             pointerNotifyFrame wlrSeat
    Right wlrXWaylandSurface -> do wlrSurface  <- G.get_wlr_surface wlrXWaylandSurface
                                   case (((unsafeCoerce wlrXWaylandSurface) == nullPtr), ((unsafeCoerce wlrSurface) == nullPtr)) of
                                     (False, False) -> do G.reference wlrSurface
                                                          safeSetActivated gsvs True -- G.set_activated wlrXWaylandSurface True
                                                          G.keyboard_notify_enter wlrSeat wlrSurface
                                                          pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
                                                          pointerNotifyFrame wlrSeat
                                     _ -> putStrLn $ "Unable to focus on sprite!"

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
  -- putStrLn "processClickEvent"
  -- Get state
  gss        <- readTVarIO (gsvs ^. gsvsServer)
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  case isMapped of
    False -> return () -- putStrLn "Surface isn't mapped!"
    True -> do let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
               (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <-
                 case wlrEitherSurface of
                   Right godotWlrXWaylandSurface -> do
                     getXWaylandSubsurfaceAndCoords gsvs godotWlrXWaylandSurface surfaceLocalCoords
                   Left godotWlrXdgSurface -> getXdgSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords
               -- -- Send events
               case evt of
                 Motion                -> do pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
                                             pointerNotifyMotion wlrSeat subSurfaceLocalCoords
                 Button pressed button -> do focus gsvs
                                             G.keyboard_notify_enter wlrSeat godotWlrSurface
                                             pointerNotifyButton wlrSeat evt

               pointerNotifyFrame wlrSeat


-- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
-- | TODO: This function just returns parent surface/coords. Fix!
-- | TODO: Use _xdg_surface_at*
getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getXdgSubsurfaceAndCoords wlrXdgSurface (SurfaceLocalCoordinates (sx, sy)) = do
  -- BROKEN since G.surface_at is broken (which causes G.get_surface to be broken).
  -- wlrSurfaceAtResult   <- G.surface_at  wlrXdgSurface sx sy -- Not NULL itself, but the wlr_surface within this data structure is NULL due to ~wlr_xdg_surface_surface_at(wlr_xdg_surface, ..)~ being NULL (but not because wlr_xdg_surface is NULL).
  -- wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult
  -- ssx                  <- G.get_sub_x wlrSurfaceAtResult
  -- ssy                  <- G.get_sub_y wlrSurfaceAtResult
  -- let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
  -- return (wlrSurfaceSubSurface, ssCoordinates)
  wlrSurfaceParent <- G.get_wlr_surface wlrXdgSurface            -- hack!
  G.reference wlrSurfaceParent
  return (wlrSurfaceParent, SubSurfaceLocalCoordinates (sx, sy)) -- hack!

keyboardNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> IO ()
keyboardNotifyEnter wlrSeat wlrSurface = do
  G.keyboard_notify_enter wlrSeat wlrSurface

-- | This function conspiciously lacks a GodotWlrSurface argument, but doesn't
-- | need one since the GodotWlrSeat keeps internal track of what the currently
-- | active surface is.
pointerNotifyMotion :: GodotWlrSeat -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyMotion wlrSeat (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  G.pointer_notify_motion wlrSeat ssx ssy
  -- putStrLn $ "G.point_notify_motion: " ++ "(" ++ (show ssx) ++ ", " ++ (show ssy) ++ ")"

pointerNotifyButton :: GodotWlrSeat -> InputEventType -> IO ()
pointerNotifyButton wlrSeat inputEventType = do
  case inputEventType of
      Motion -> return ()
      Button pressed buttonIndex -> pointerNotifyButton' pressed buttonIndex
  where pointerNotifyButton' pressed buttonIndex = do
          G.pointer_notify_button wlrSeat (fromIntegral buttonIndex) pressed
          -- putStrLn $ "G.pointer_notify_button: pressed/buttonIndex" ++ (show pressed) ++ "/" ++ (show buttonIndex)
          return ()

-- | Sends a frame event to the surface with pointer focus (apparently
-- | useful in particular for axis events); unclear if this is needed in VR but we
-- | use it regardless.
pointerNotifyFrame :: GodotWlrSeat -> IO ()
pointerNotifyFrame wlrSeat = do
  G.pointer_notify_frame wlrSeat

-- | Let wlroots know we have entered a new surface. We can safely call this
-- | over and over (wlroots checks if we've called it already for this surface
-- | and, if so, returns early.
pointerNotifyEnter :: GodotWlrSeat -> GodotWlrSurface -> SubSurfaceLocalCoordinates -> IO ()
pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (ssx, ssy)) = do
  G.pointer_notify_enter wlrSeat wlrSurface ssx ssy -- Causing a crash

_handle_map :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_map gsvs args = do
  putStrLn $ "_handle_map"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    Left wlrXdgSurface -> return ()
    Right wlrXWaylandSurface -> do -- Safeguard to prevent potentially weird behavior
                                   zero <- toLowLevel (V2 0 0)
                                   G.set_xy wlrXWaylandSurface zero

  G.set_process gsvs True
  G.set_process_input gsvs True
  emitSignal gsvs "map" ([gsvs] :: [GodotSimulaViewSprite])
  atomically $ writeTVar (simulaView ^. svMapped) True
  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)

  -- Flag the gsvs as a starting application, if it is one
  appCounter@(appsLaunched, appsRemaining) <- readTVarIO (gss ^. gssStartingAppsCounter)
  case appsRemaining of
    0 -> return ()
    _ -> do atomically $ writeTVar (_gsvsShouldMove gsvs) (True, appsLaunched + 1)
            atomically $ writeTVar (_gssStartingAppsCounter gss) (appsLaunched + 1, appsRemaining - 1)

  return ()

handle_unmap :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap self args@[wlrXWaylandSurfaceVariant] = do
  handle_unmap_base self args
  simulaView <- atomically $ readTVar (self ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    Left wlrXdgSurface -> return ()
    Right wlrXWaylandSurface -> do G.send_close wlrXWaylandSurface -- Fixes ulauncher crashes

-- Passes control entirely to updateSimulaViewSprite.
_process :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_process self _ = do
  simulaView <- readTVarIO (self ^. gsvsView)
  mapped <- atomically $ readTVar (simulaView ^. svMapped)
  when mapped $ updateSimulaViewSprite self
  return ()


-- The original Simula didn't have a destroy handler at the Godot level.
-- 1. Was it leaking?
-- 2. Do Godot objects get deleted eventually anyway, even if we don't call queue_free?
-- 3. I'm assuming that `registerClass` passes a destructor for
--    GodotSimulaViewSprite that calls to `queue_free` can use.
_handle_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_handle_destroy gsvs [gsvsGV] = do
  putStrLn "_handle_destroy"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  gss <- readTVarIO (gsvs ^. gsvsServer)

  -- Destroy
  G.queue_free gsvs -- Queue the `gsvs` for destruction
  G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
  atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
    -- Old method of gsvs deletion from Simula; bring back if we face leakage issues:
  -- Api.godot_object_destroy (safeCast gsvs)
  deleteSurface eitherSurface

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
  orientSpriteTowardsGaze gsvs
  pushBackVector <- toLowLevel (V3 0 0 dist) :: IO GodotVector3 -- For some reason we also have to shift the vector 0.5 units to the right
  G.translate_object_local gsvs pushBackVector
  return ()

-- Sets gssKeyboardGrabbedSprite to `Just (gsvs, dist)`
keyboardGrabInitiate :: GodotSimulaViewSprite -> IO ()
keyboardGrabInitiate gsvs = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast gsvs) :: GodotNode)
  case isInSceneGraph of
    False -> keyboardGrabLetGo gsvs
    True -> do gss <- readTVarIO $ (gsvs ^. gsvsServer)
               -- Compute dist
               orientSpriteTowardsGaze gsvs
               posGSVS <- (G.get_global_transform gsvs) >>= Api.godot_transform_get_origin
               hmdTransform <- getARVRCameraOrPancakeCameraTransform gss
               posHMD  <- Api.godot_transform_get_origin hmdTransform
               dist <- realToFrac <$> Api.godot_vector3_distance_to posGSVS posHMD
               -- Load state
               atomically $ writeTVar (gss ^. gssKeyboardGrabbedSprite) (Just (gsvs, (-dist)))
  return ()

-- Sets gssKeyboardGrabbedSprite to `Nothing`
keyboardGrabLetGo :: GodotSimulaViewSprite -> IO ()
keyboardGrabLetGo gsvs = do
  gss <- readTVarIO $ (gsvs ^. gsvsServer)
  atomically $ writeTVar (gss ^. gssKeyboardGrabbedSprite) Nothing

setInFrontOfUser :: GodotSimulaViewSprite -> Float -> IO ()
setInFrontOfUser gsvs zAxisDist = do
  gsvsScale <- G.get_scale (safeCast gsvs :: GodotSpatial)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  rotationAxisY <- toLowLevel (V3 0 1 0) :: IO GodotVector3
  pushBackVector <- toLowLevel (V3 0 0 zAxisDist) :: IO GodotVector3
  hmdGlobalTransform <- getARVRCameraOrPancakeCameraTransform gss
  G.set_global_transform gsvs hmdGlobalTransform
  G.translate_object_local gsvs pushBackVector
  G.rotate_object_local gsvs rotationAxisY 3.14159
  G.scale_object_local (safeCast gsvs :: GodotSpatial) gsvsScale

safeSetActivated :: GodotSimulaViewSprite -> Bool -> IO ()
safeSetActivated gsvs active = do
  simulaView <- readTVarIO $ (gsvs ^. gsvsView)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  case (isMapped, wlrEitherSurface) of
    (True, Right wlrXWaylandSurface) -> G.set_activated wlrXWaylandSurface True
    (False, _) -> return ()
    (_, Left wlrXdgSurface) -> logStr "ERROR: XDG surfaces not supported by Simula at the moment."

safeSurfaceAt :: GodotSimulaViewSprite -> Float -> Float -> IO (Maybe GodotWlrSurfaceAtResult)
safeSurfaceAt gsvs sx sy = do
  simulaView <- readTVarIO $ (gsvs ^. gsvsView)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  ret <- case (isMapped, wlrEitherSurface) of
            (True, Right wlrXWaylandSurface) -> do surfaceAtRes <- G.surface_at wlrXWaylandSurface sx sy :: IO GodotWlrSurfaceAtResult
                                                   return (Just surfaceAtRes)
            _ -> return Nothing
  return ret

applyViewportBaseTexture :: GodotSimulaViewSprite -> IO ()
applyViewportBaseTexture gsvs = do
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurface <- getWlrSurface eitherSurface
  meshInstance <- readTVarIO (gsvs ^. gsvsMeshInstance)
  quadMesh <- getQuadMesh gsvs
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  viewportBase <- readTVarIO (cb ^. cbViewport)
  viewportBaseTexture <- G.get_texture viewportBase

  shm <- G.get_material quadMesh >>= asClass' GodotShaderMaterial "ShaderMaterial" :: IO GodotShaderMaterial

  viewportBaseTextureGV <- (toLowLevel (toVariant ((safeCast viewportBaseTexture) :: GodotObject))) :: IO GodotVariant
  texture_albedo <- toLowLevel (pack "texture_albedo") :: IO GodotString
  G.set_shader_param shm texture_albedo viewportBaseTextureGV

handle_map_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_map_free_child gsvsInvisible [wlrXWaylandSurfaceVariant] = do
  putStrLn $ "handle_map_free_child"
  gss <- readTVarIO $ (gsvsInvisible ^. gsvsServer)
  wlrXWaylandSurface <- fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  case maybeActiveCursorGSVS of
     Nothing -> putStrLn "Cannot map free child!"
     Just gsvs -> do maybeSurfaceLocalCoords <- computeSurfaceLocalCoordinates gsvs wlrXWaylandSurface
                     case maybeSurfaceLocalCoords of
                       Nothing -> return ()
                       Just (sx, sy) -> do -- Push surface onto end of gsvsFreeChildren stack
                                           x <- G.get_x wlrXWaylandSurface
                                           y <- G.get_y wlrXWaylandSurface
                                           let xOffset = (sx - x)
                                           let yOffset = (sy - y)

                                           adjustedXY <- getAdjustedXYFreeChild gsvs wlrXWaylandSurface
                                           adjustedXY'@(V2 x' y') <- fromLowLevel adjustedXY :: IO (V2 Float)
                                           G.set_xy wlrXWaylandSurface adjustedXY

                                           let sx' = (if (round x') == (fromIntegral x) then sx else (round x'))
                                           let sy' = (if (round y') == (fromIntegral y) then sy else (round y'))

                                           G.reference wlrXWaylandSurface
                                           wlrSurface <- G.get_wlr_surface wlrXWaylandSurface
                                           cs <- newCanvasSurface gsvs (wlrSurface, sx', sy')
                                           freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
                                           atomically $ writeTVar (gsvs ^. gsvsFreeChildren) (freeChildren ++ [cs])

                                           -- Add surface into gssFreeChildren map
                                           freeChildrenMapOld <- readTVarIO (gss ^. gssFreeChildren) :: IO (M.Map GodotWlrXWaylandSurface CanvasSurface)
                                           let freeChildrenMapNew = M.insert wlrXWaylandSurface cs freeChildrenMapOld
                                           atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew

  simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
  atomically $ writeTVar (simulaView ^. svMapped) True
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = do
          localX <- G.get_x child
          localY <- G.get_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> return Nothing
                          Right parent -> do globalX <- G.get_x parent
                                             globalY <- G.get_y parent
                                             return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

getAdjustedXY :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO GodotVector2
getAdjustedXY gsvs wlrXWaylandSurface = do
  childSX <- G.get_x wlrXWaylandSurface
  childSY <- G.get_y wlrXWaylandSurface
  childWidth <- G.get_width wlrXWaylandSurface
  childHeight <- G.get_height wlrXWaylandSurface
  maybeSpriteDims <- readTVarIO (gsvs ^. gsvsTargetSize)

  (parentWidth, parentHeight) <- case maybeSpriteDims of
                     Nothing -> do
                        simulaView <- readTVarIO (gsvs ^. gsvsView)
                        let eitherSurface = (simulaView ^. svWlrEitherSurface)
                        wlrSurface <- getWlrSurface eitherSurface
                        (bx, by) <- getBufferDimensions wlrSurface
                        return (bx, by)
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
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  (V2 parentX parentY) <- case eitherSurface of
                               (Left wlrXdgSurface) -> return (V2 0 0)
                               (Right wlrXWaylandSurfaceParent) -> do x <- G.get_x wlrXWaylandSurfaceParent
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
                        wlrSurface <- getWlrSurface eitherSurface
                        (bx, by) <- getBufferDimensions wlrSurface
                        putStrLn $ "(bx, by): " ++ (show bx) ++ ", " ++ (show by)
                        return (bx, by)
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
  simulaViewMap <- readTVarIO (gss ^. gssViews)
  parentWlrXWaylandSurface <- G.get_parent wlrXWaylandSurface -- wlr_xwayland_surface->parent
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
handle_map_child gsvsInvisible args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "handle_map_child"
  gss <- readTVarIO $ (gsvsInvisible ^. gsvsServer)
  wlrXWaylandSurface <- fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface
  gss <- readTVarIO (gsvsInvisible ^. gsvsServer)
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> do
      -- Since this surface's parent isn't known, we route to `handle_map_free_child`
      putStrLn "handle_map_child: re-routing to handle_map_free_child"
      handle_map_free_child gsvsInvisible args
    Just (parentGSVS) -> do adjustedXY <- getAdjustedXY parentGSVS wlrXWaylandSurface
                            G.set_xy wlrXWaylandSurface adjustedXY
                            simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
                            atomically $ writeTVar (simulaView ^. svMapped) True
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = do
          localX <- G.get_x child
          localY <- G.get_y child

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          maybeCoords <- case eitherSurface of
                          Left wlrXdgSurface -> return Nothing
                          Right parent -> do globalX <- G.get_x parent
                                             globalY <- G.get_y parent
                                             return $ Just (localX - globalX, localY - globalY)
          return maybeCoords

handle_set_parent :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_set_parent gsvs [wlrXWaylandSurfaceVariant] = do
  -- Intentionally empty for now
  wlrXWaylandSurface <- fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface
  return ()

handle_unmap_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_child self args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "_handle_unmap_child"
  handle_unmap_base self args

handle_unmap_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_free_child self args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "_handle_unmap_free_child"
  handle_unmap_base self args

handle_unmap_base :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_base self [wlrXWaylandSurfaceVariant] = do
  putStrLn "handle_unmap_base"
  gss <- readTVarIO (self ^. gsvsServer)
  simulaView <- atomically $ readTVar (self ^. gsvsView)
  freeChildrenMap <- readTVarIO (gss ^. gssFreeChildren)
  wlrXWaylandSurface <- fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface

  keyboardGrabLetGo self

  G.reference wlrXWaylandSurface
  let maybeCS = M.lookup wlrXWaylandSurface freeChildrenMap
  case maybeCS of
    Just cs -> do -- Delete the free child from the parentGSVS and the gssFreeChildren map
                  putStrLn "handle_unmap_base: free child path"
                  parentGSVS <- readTVarIO (cs ^. csGSVS)
                  freeChildren <- readTVarIO (parentGSVS ^. gsvsFreeChildren)
                  let freeChildrenNew = Data.List.delete cs freeChildren
                  atomically $ writeTVar (parentGSVS ^. gsvsFreeChildren) freeChildrenNew
                  let freeChildrenMapNew = M.delete wlrXWaylandSurface freeChildrenMap
                  atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew

                  -- Destroy and prevent drawing
                  G.set_process cs False
                  G.queue_free cs


    Nothing -> do putStrLn "handle_unmap_base: normal child path"
                  simulaView <- readTVarIO (self ^. gsvsView)
                  atomically $ writeTVar (simulaView ^. svMapped) False

                  -- Ensure that we de-focus the gsvs if it is active
                  gss <- readTVarIO (self ^. gsvsServer)
                  maybeGSVSFocused <- readTVarIO (gss ^. gssKeyboardFocusedSprite)
                  case maybeGSVSFocused of
                    Nothing -> return ()
                    (Just gsvsFocused) -> do
                      simulaViewFocused <- readTVarIO (gsvsFocused ^. gsvsView)
                      if (simulaViewFocused == simulaView) then (atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) Nothing)
                                                          else (return ())

  G.set_process self False
  atomically $ writeTVar (simulaView ^. svMapped) False
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast self) :: GodotNode)
  case isInSceneGraph of
       True -> removeChild gss self
       False -> return ()

getXWaylandSubsurfaceAndCoords :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getXWaylandSubsurfaceAndCoords gsvs wlrXWaylandSurface coords@(SurfaceLocalCoordinates (sx, sy)) = do
  xRet@(wlrSurface', SubSurfaceLocalCoordinates (subX, subY)) <- withGodot
    (G.surface_at wlrXWaylandSurface sx sy)
    (destroyMaybe . safeCast)
    (\wlrSurfaceAtResult -> do G.reference wlrSurfaceAtResult
                               subX <- G.get_sub_x wlrSurfaceAtResult
                               subY <- G.get_sub_y wlrSurfaceAtResult
                               wlrSurface' <- G.get_surface wlrSurfaceAtResult
                               return (wlrSurface', SubSurfaceLocalCoordinates (subX, subY)))
  freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
  maybeFreeRet <- getFreeChildrenCoords wlrXWaylandSurface coords freeChildren
  case maybeFreeRet of
     Just freeSurfaceAndCoords@(wlrSurface, SubSurfaceLocalCoordinates (x, y), maybeWlrXWaylandSurface) -> do
       let wlrXWaylandSurface = Data.Maybe.fromJust maybeWlrXWaylandSurface
       G.set_activated wlrXWaylandSurface True
       return (wlrSurface, SubSurfaceLocalCoordinates (x, y))
     Nothing -> do
       safeSetActivated gsvs True -- G.set_activated godotWlrXWaylandSurface True
       return xRet

getFreeChildCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> CanvasSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildCoords parentWlrXWaylandSurface (SurfaceLocalCoordinates (cx, cy)) freeChildCS = do
  gsvsFreeChild <- readTVarIO (freeChildCS ^. csGSVS)
  simulaView <- readTVarIO (gsvsFreeChild ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  let maybeWlrXWaylandSurface = case eitherSurface of
        Left xdg -> Nothing
        Right wlrXWaylandSurface -> Just wlrXWaylandSurface

  (freeChildSurface, fsx', fsy') <- readTVarIO (freeChildCS ^. csSurface)
  let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
  (fLengthX', fLengthY') <- getBufferDimensions freeChildSurface
  let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
  case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
    (True, True, True, True) -> do
      let x = cx - fsx
      let y = cy - fsy
      return $ Just (freeChildSurface, SubSurfaceLocalCoordinates (x, y), maybeWlrXWaylandSurface)
    _ -> do return Nothing

getFreeChildrenCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> [CanvasSurface] -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildrenCoords parentWlrXWaylandSurface coords@(SurfaceLocalCoordinates (cx, cy)) freeChildrenCS = do
  let freeChildrenCS' = Data.List.reverse freeChildrenCS
  case (freeChildrenCS' == []) of
    True -> return Nothing
    False -> do let freeChildHead = Data.List.head freeChildrenCS'
                maybeFreeChildCoords <- getFreeChildCoords parentWlrXWaylandSurface coords freeChildHead
                case maybeFreeChildCoords of
                     Nothing -> getFreeChildrenCoords parentWlrXWaylandSurface coords (Data.List.tail $ Data.List.reverse $ freeChildrenCS')
                     Just ret -> return $ Just ret