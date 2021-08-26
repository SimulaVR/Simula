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

import System.Process

instance Eq GodotSimulaViewSprite where
  (==) = (==) `on` _gsvsObj

instance NativeScript GodotSimulaViewSprite where
  className = "SimulaViewSprite"
  classInit obj =
    do putStrLn "SimulaViewSprite()"
       GodotSimulaViewSprite (safeCast obj)
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
                      <*> atomically (newTVar [])
                      <*> atomically (newTVar False)
  -- classExtends = "RigidBody"
  classMethods =
    [ func NoRPC "_input_event" inputEvent
    , func NoRPC "_ready" ready

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
  setTargetDimensions gsvs
  applyViewportBaseTexture gsvs
  setBoxShapeExtentsToMatchAABB gsvs

  whenM (spriteReadyToMove gsvs) $ do
      simulaView <- readTVarIO (gsvs ^. gsvsView) --
      let eitherSurface = (simulaView ^. svWlrEitherSurface)
      gss <- readTVarIO (gsvs ^. gsvsServer)
      pid <- case eitherSurface of
                  Left wlrXdgSurface -> do
                    wlrXdgSurface <- validateSurfaceE wlrXdgSurface
                    pidInt <- G.get_pid wlrXdgSurface
                    return $ (fromInteger $ fromIntegral pidInt)
                  Right wlrXWaylandSurface -> do
                    wlrXWaylandSurface <- validateSurfaceE wlrXWaylandSurface
                    pidInt <- G.get_pid wlrXWaylandSurface
                    return $ (fromInteger $ fromIntegral pidInt)
      pids <- getParentsPids pid
      maybeLocation <- getSimulaStartingLocationAtomically gss pids
      case maybeLocation of
        Just location -> moveToStartingPosition gsvs location
        Nothing -> return ()
      atomically $ writeTVar (_gsvsShouldMove gsvs) False
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
  shouldMove <- atomically $ readTVar (_gsvsShouldMove gsvs)
  if shouldMove then do meshInstance <- atomically $ readTVar (_gsvsMeshInstance gsvs)
                        aabb <- G.get_aabb meshInstance
                        size <- godot_aabb_get_size aabb
                        vsize <- fromLowLevel size
                        return (vsize > 0) -- The first frame or so, the sprite has vsize 0
                   else return False

moveToStartingPosition :: GodotSimulaViewSprite -> String -> IO ()
moveToStartingPosition gsvs appLocation = do
  gss <- readTVarIO (gsvs ^. gsvsServer)
  meshInstance <- atomically $ readTVar (gsvs ^. gsvsMeshInstance)
  aabb   <- G.get_aabb meshInstance
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
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
                   G.translate gsvs =<< toLowLevel (V3 0 (-sizeX) 0)
    "left" -> do G.set_global_transform gsvs startingAppTransform'
                 G.translate gsvs =<< toLowLevel (V3 (sizeX) 0 0)
    "top" -> do G.set_global_transform gsvs startingAppTransform'
                G.translate gsvs =<< toLowLevel (V3 0 (sizeX) 0)
    _ -> return ()
  orientSpriteTowardsGaze gsvs

-- Sets gsvs wlr_xwayland_surface size and all associated viewports to the
-- gsvsTargetSize every frame
setTargetDimensions :: GodotSimulaViewSprite -> IO ()
setTargetDimensions gsvs = do
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  renderTargetBase <- readTVarIO (cb ^. cbViewport)
  cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
  renderTargetSurface <- readTVarIO (cs ^. csViewport)

  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  wlrSurface <- (getWlrSurface eitherSurface) >>= validateSurfaceE

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
  quadMesh <- getQuadMesh gsvs
  -- QuadMesh need to be scaled down by a factor of 0.001 to be reasonably sized in Godot:
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
                           case (transOld == 1) of
                             True -> do
                               setShader gsvs "res://addons/godot-haskell-plugin/TextShaderOpaque.tres"
                               atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
                             False -> return ()
        (True, False) -> do let pushX = spilloverWidth - oldSpilloverWidth
                            let pushY = spilloverHeight - oldSpilloverHeight
                            pushBackVector <- toLowLevel (V3 (-0.5 * 0.001 * (fromIntegral pushX)) (-0.5 * 0.001 * (fromIntegral pushY)) 0) :: IO GodotVector3
                            G.translate_object_local gsvs pushBackVector
                            atomically $ writeTVar (gsvs ^. gsvsSpilloverDims) (Just spilloverDims)
                            case (transOld == 1, (spilloverWidth > targetWidth || spilloverHeight > targetHeight)) of
                              (True, False) ->  return () -- Avoid changing shader when apps first launch
                              (True, True) -> do setShader gsvs "res://addons/godot-haskell-plugin/TextShader.tres"
                                                 atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
                              (False, _) -> return ()
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
  keyboardShortcuts <- readTVarIO (gss ^. gssKeyboardShortcuts)
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

focus :: GodotSimulaViewSprite -> IO ()
focus gsvs = do
  simulaView  <- atomically $ readTVar (gsvs ^. gsvsView)
  gss         <- atomically $ readTVar (gsvs ^. gsvsServer)
  wlrSeat     <- atomically $ readTVar (gss ^. gssWlrSeat)
  let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)

  atomically $ writeTVar (gss ^. gssKeyboardFocusedSprite) (Just gsvs)
  atomically $ writeTVar (gss ^. gssActiveCursorGSVS) (Just gsvs)

  case wlrEitherSurface of
    Left wlrXdgSurface -> do validateSurfaceE wlrXdgSurface
                             wlrSurface  <- (G.get_wlr_surface wlrXdgSurface) >>= validateSurfaceE
                             G.reference wlrSurface
                             toplevel    <- G.get_xdg_toplevel wlrXdgSurface :: IO GodotWlrXdgToplevel
                             -- isGodotTypeNull wlrSurface
                             G.set_activated toplevel True
                             G.keyboard_notify_enter wlrSeat wlrSurface
                             pointerNotifyEnter wlrSeat wlrSurface (SubSurfaceLocalCoordinates (0,0))
                             pointerNotifyFrame wlrSeat
    Right wlrXWaylandSurface -> do validateSurfaceE wlrXWaylandSurface
                                   wlrSurface <- G.get_wlr_surface wlrXWaylandSurface >>= validateSurfaceE
                                   G.reference wlrSurface
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
  wlrSeat    <- readTVarIO (gss ^. gssWlrSeat)
  simulaView <- readTVarIO (gsvs ^. gsvsView)

  isMapped <- readTVarIO $ (simulaView ^. svMapped)
  case isMapped of
    False -> putStrLn "processClickevent' isMapped is false!"
    True -> do let wlrEitherSurface = (simulaView ^. svWlrEitherSurface)
               (godotWlrSurface, subSurfaceLocalCoords@(SubSurfaceLocalCoordinates (ssx, ssy))) <-
                 case wlrEitherSurface of
                   Right godotWlrXWaylandSurface -> do
                     getXWaylandSubsurfaceAndCoords gsvs godotWlrXWaylandSurface surfaceLocalCoords
                   Left godotWlrXdgSurface -> getXdgSubsurfaceAndCoords godotWlrXdgSurface surfaceLocalCoords
               wlrSeat <- readTVarIO (gss ^. gssWlrSeat)
               pointerNotifyEnter wlrSeat godotWlrSurface subSurfaceLocalCoords
               pointerNotifyMotion wlrSeat subSurfaceLocalCoords

               case evt of
                 Motion                -> do return ()
                 Button pressed button -> do ---focus gsvs
                                             G.keyboard_notify_enter wlrSeat godotWlrSurface
                                             pointerNotifyButton wlrSeat evt
                                             case pressed of
                                               False -> G.pointer_clear_focus wlrSeat
                                               True -> return ()

               pointerNotifyFrame wlrSeat

-- | Takes a GodotWlrXdgSurface and returns the subsurface at point (which is likely the surface itself, or one of its popups).
-- | TODO: This function just returns parent surface/coords. Fix!
-- | TODO: Use _xdg_surface_at*
getXdgSubsurfaceAndCoords :: GodotWlrXdgSurface -> SurfaceLocalCoordinates -> IO (GodotWlrSurface, SubSurfaceLocalCoordinates)
getXdgSubsurfaceAndCoords wlrXdgSurface cursorCoords@(SurfaceLocalCoordinates (sx, sy)) = do
  rect2@(V2 (V2 posX posY) (V2 xdgWidth xdgHeight)) <- G.get_geometry wlrXdgSurface >>= fromLowLevel :: IO (V2 (V2 Float))
  wlrSurfaceAtResult   <- G.surface_at wlrXdgSurface sx sy
  wlrSurfaceSubSurface <- G.get_surface wlrSurfaceAtResult >>= validateSurfaceE
  G.reference wlrSurfaceSubSurface
  ssx                  <- G.get_sub_x wlrSurfaceAtResult
  ssy                  <- G.get_sub_y wlrSurfaceAtResult
  let ssCoordinates    = SubSurfaceLocalCoordinates (ssx, ssy)
  return (wlrSurfaceSubSurface, SubSurfaceLocalCoordinates (ssx, ssy))

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
_handle_map gsvs _ = do
  putStrLn $ "_handle_map"
  gss <- readTVarIO (gsvs ^. gsvsServer)
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  case eitherSurface of
    Left wlrXdgSurface -> do
      validateSurfaceE wlrXdgSurface
      putStrLn $ "Refraining from setting wlrXdgSurface position"
    Right wlrXWaylandSurface -> do -- Safeguard to prevent potentially weird behavior
                                   validateSurfaceE wlrXWaylandSurface
                                   zero <- toLowLevel (V2 0 0)
                                   G.set_xy wlrXWaylandSurface zero

  G.set_process gsvs True
  G.set_process_input gsvs True
  atomically $ modifyTVar' (_gssViews gss) (M.insert simulaView gsvs) -- TVar (M.Map SimulaView GodotSimulaViewSprite)
  atomically $ writeTVar (_gsvsShouldMove gsvs) True

  putStr "Mapping surface "
  print (safeCast @GodotObject gsvs)
  -- Add the gsvs as a child to the current workspace
  (workspace, workspaceStr) <- readTVarIO (gss ^. gssWorkspace)
  G.add_child ((safeCast workspace) :: GodotNode)
              ((safeCast gsvs)      :: GodotNode)
              True

  cb <- newCanvasBase gsvs
  viewportBase <- readTVarIO (cb ^. cbViewport)
  atomically $ writeTVar (gsvs ^. gsvsCanvasBase) cb
  G.set_process cb True
  addChild gsvs viewportBase
  addChild viewportBase cb

  cs <- newCanvasSurface gsvs
  viewportSurface <- readTVarIO (cs ^. csViewport)
  atomically $ writeTVar (gsvs ^. gsvsCanvasSurface) cs
  G.set_process cs True
  addChild gsvs viewportSurface
  addChild viewportSurface cs

  setInFrontOfUser gsvs (-2)

  V3 1 1 1 ^* (1 + 1 * 1) & toLowLevel >>= G.scale_object_local (safeCast gsvs :: GodotSpatial)

  focus gsvs -- We're relying on this to add references to wlrSurface :/

  atomically $ writeTVar (simulaView ^. svMapped) True

  return ()

handle_unmap :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap self args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "handle_unmap"
  handle_unmap_base self args

-- Passes control entirely to updateSimulaViewSprite.
_process :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
_process self _ = do
  simulaView <- readTVarIO (self ^. gsvsView)
  mapped <- atomically $ readTVar (simulaView ^. svMapped)
  isAtTargetDims <- readTVarIO (self ^. gsvsIsAtTargetDims)
  case (isAtTargetDims, mapped) of
    (False, True) -> do
      isAtTargetDimsNow <- isAtTargetDimensions self
      if isAtTargetDimsNow then (atomically $ writeTVar (self ^. gsvsIsAtTargetDims) True) else (return ())
    (True, True) -> updateSimulaViewSprite self
    _ -> return ()
  return ()
  where isAtTargetDimensions :: GodotSimulaViewSprite -> IO Bool
        isAtTargetDimensions gsvs = do
          cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
          renderTargetBase <- readTVarIO (cb ^. cbViewport)
          cs <- readTVarIO (gsvs ^. gsvsCanvasSurface)
          renderTargetSurface <- readTVarIO (cs ^. csViewport)

          simulaView <- readTVarIO (gsvs ^. gsvsView)
          let eitherSurface = (simulaView ^. svWlrEitherSurface)
          wlrSurface <- (getWlrSurface eitherSurface) >>= validateSurfaceE

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
_handle_destroy gsvs [gsvsGV] = do
  putStrLn "_handle_destroy"
  simulaView <- readTVarIO (gsvs ^. gsvsView)
  let eitherSurface = (simulaView ^. svWlrEitherSurface)
  gss <- readTVarIO (gsvs ^. gsvsServer)
  maybeActiveCursorGSVS <- readTVarIO (gss ^. gssActiveCursorGSVS)
  case maybeActiveCursorGSVS of
    Just activeCursorGSVS -> case (gsvs == activeCursorGSVS) of
                                  True -> do atomically $ writeTVar (gss ^. gssActiveCursorGSVS) Nothing
                                  False -> return ()
    Nothing -> return ()

  G.queue_free gsvs -- Queue the `gsvs` for destruction
  G.set_process gsvs False -- Remove the `simulaView ↦ gsvs` mapping from the gss
  atomically $ modifyTVar' (gss ^. gssViews) (M.delete simulaView)
  deleteSurface eitherSurface -- Causing issues with rr

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

setInFrontOfUser :: GodotSimulaViewSprite -> Float -> IO ()
setInFrontOfUser gsvs zAxisDist = do
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
  wlrSurface <- (getWlrSurface eitherSurface) >>= validateSurfaceE
  meshInstance <- readTVarIO (gsvs ^. gsvsMeshInstance)
  quadMesh <- getQuadMesh gsvs
  cb <- readTVarIO (gsvs ^. gsvsCanvasBase)
  viewportBase <- readTVarIO (cb ^. cbViewport)
  viewportBaseTexture <- G.get_texture viewportBase

  shm <- G.get_material quadMesh >>= asClass' GodotShaderMaterial "ShaderMaterial" :: IO GodotShaderMaterial

  viewportBaseTextureGV <- (toLowLevel (toVariant ((safeCast viewportBaseTexture) :: GodotObject))) :: IO GodotVariant
  texture_albedo <- toLowLevel (pack "texture_albedo") :: IO GodotString
  G.set_shader_param shm texture_albedo viewportBaseTextureGV
  Api.godot_variant_destroy viewportBaseTextureGV
  Api.godot_string_destroy texture_albedo

handle_map_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_map_free_child gsvsInvisible [wlrXWaylandSurfaceVariant] = do
  gss <- readTVarIO $ (gsvsInvisible ^. gsvsServer)
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
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

                                           -- adjustedXY <- getAdjustedXYFreeChild gsvs wlrXWaylandSurface
                                           -- adjustedXY'@(V2 x' y') <- fromLowLevel adjustedXY :: IO (V2 Float)
                                           -- G.set_xy wlrXWaylandSurface adjustedXY

                                           -- let sx' = (if (round x') == (fromIntegral x) then sx else (round x'))
                                           -- let sy' = (if (round y') == (fromIntegral y) then sy else (round y'))

                                           G.reference wlrXWaylandSurface
                                           wlrSurface <- (G.get_wlr_surface wlrXWaylandSurface) >>= validateSurfaceE
                                           freeChildren <- readTVarIO (gsvs ^. gsvsFreeChildren)
                                           atomically $ writeTVar (gsvs ^. gsvsFreeChildren) (freeChildren ++ [wlrXWaylandSurface])

                                           -- Add surface into gssFreeChildren map
                                           freeChildrenMapOld <- readTVarIO (gss ^. gssFreeChildren) :: IO (M.Map GodotWlrXWaylandSurface GodotSimulaViewSprite)
                                           let freeChildrenMapNew = M.insert wlrXWaylandSurface gsvs freeChildrenMapOld
                                           atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew
                                           atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True

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
  childSX <- G.get_x wlrXWaylandSurface
  childSY <- G.get_y wlrXWaylandSurface
  childWidth <- G.get_width wlrXWaylandSurface
  childHeight <- G.get_height wlrXWaylandSurface
  maybeSpriteDims <- readTVarIO (gsvs ^. gsvsTargetSize)

  (parentWidth, parentHeight) <- case maybeSpriteDims of
                     Nothing -> do
                        simulaView <- readTVarIO (gsvs ^. gsvsView)
                        let eitherSurface = (simulaView ^. svWlrEitherSurface)
                        wlrSurface <- (getWlrSurface eitherSurface) >>= validateSurfaceE
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
                        wlrSurface <- (getWlrSurface eitherSurface) >>= validateSurfaceE
                        (bx, by) <- getBufferDimensions wlrSurface
                        -- putStrLn $ "(bx, by): " ++ (show bx) ++ ", " ++ (show by)
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
  parentWlrXWaylandSurface <- (G.get_parent wlrXWaylandSurface) >>= validateSurfaceE
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
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  gss <- readTVarIO (gsvsInvisible ^. gsvsServer)
  maybeParentGSVS <- getParentGSVS gss wlrXWaylandSurface
  case maybeParentGSVS of
    Nothing -> do
      -- Since this surface's parent isn't known, we route to `handle_map_free_child`
      putStrLn "handle_map_child: re-routing to handle_map_free_child"
      handle_map_free_child gsvsInvisible args
    Just (parentGSVS) -> do -- adjustedXY <- getAdjustedXY parentGSVS wlrXWaylandSurface
                            -- G.set_xy wlrXWaylandSurface adjustedXY
                            simulaView <- readTVarIO (gsvsInvisible ^. gsvsView)
                            atomically $ writeTVar (simulaView ^. svMapped) True
                            atomically $ writeTVar (gsvsInvisible ^. gsvsIsDamaged) True
  where
        computeSurfaceLocalCoordinates :: GodotSimulaViewSprite -> GodotWlrXWaylandSurface -> IO (Maybe (Int, Int))
        computeSurfaceLocalCoordinates gsvs child = do
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
handle_set_parent gsvs [wlrXWaylandSurfaceVariant] = do
  -- Intentionally empty for now
  wlrXWaylandSurface <- (fromGodotVariant wlrXWaylandSurfaceVariant :: IO GodotWlrXWaylandSurface) >>= validateSurfaceE
  return ()

handle_unmap_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_child self args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "_handle_unmap_child"
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self args

handle_unmap_free_child :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_free_child self args@[wlrXWaylandSurfaceVariant] = do
  putStrLn "_handle_unmap_free_child"
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
  handle_unmap_base self args

handle_unmap_base :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_unmap_base self [wlrXWaylandSurfaceVariant] = do
  putStrLn "handle_unmap_base"
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
                  putStrLn "handle_unmap_base: free child path"
                  freeChildren <- readTVarIO (parentGSVS ^. gsvsFreeChildren)
                  let freeChildrenNew = Data.List.delete wlrXWaylandSurface freeChildren
                  atomically $ writeTVar (parentGSVS ^. gsvsFreeChildren) freeChildrenNew
                  let freeChildrenMapNew = M.delete wlrXWaylandSurface freeChildrenMap
                  atomically $ writeTVar (gss ^. gssFreeChildren) freeChildrenMapNew


    Nothing -> do putStrLn "handle_unmap_base: normal child path"
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
  atomically $ writeTVar (simulaView ^. svMapped) False
  isInSceneGraph <- G.is_a_parent_of ((safeCast gss) :: GodotNode ) ((safeCast self) :: GodotNode)
  atomically $ writeTVar (self ^. gsvsIsDamaged) True
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
                               wlrSurface' <- (G.get_surface wlrSurfaceAtResult) >>= validateSurfaceE
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

getFreeChildCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> GodotWlrXWaylandSurface -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildCoords parentWlrXWaylandSurface (SurfaceLocalCoordinates (cx, cy)) wlrXWaylandSurface = do
  freeChildSurface <- G.get_wlr_surface wlrXWaylandSurface
  fsx' <- G.get_x wlrXWaylandSurface
  fsy' <- G.get_y wlrXWaylandSurface

  let (fsx, fsy) = (fromIntegral fsx', fromIntegral fsy')
  (fLengthX', fLengthY') <- getBufferDimensions freeChildSurface
  let (fLengthX, fLengthY) = (fromIntegral fLengthX', fromIntegral fLengthY')
  case (fsx < cx, cx < fsx + fLengthX, fsy < cy, cy < fsy + fLengthY) of
    (True, True, True, True) -> do
      let x = cx - fsx
      let y = cy - fsy
      return $ Just (freeChildSurface, SubSurfaceLocalCoordinates (x, y), Just wlrXWaylandSurface)
    _ -> do return Nothing

getFreeChildrenCoords :: GodotWlrXWaylandSurface -> SurfaceLocalCoordinates -> [GodotWlrXWaylandSurface] -> IO (Maybe (GodotWlrSurface, SubSurfaceLocalCoordinates, Maybe GodotWlrXWaylandSurface))
getFreeChildrenCoords parentWlrXWaylandSurface coords@(SurfaceLocalCoordinates (cx, cy)) freeChildren = do
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
handle_new_popup gsvs args@[wlrXdgSurfaceParentVariant] = do
  putStrLn "handle_new_popup"
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  return ()

handle_window_menu :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_window_menu gsvsInvisible args@[wlrXdgToplevel, serial, x, y] = do
  atomically $ writeTVar (gsvsInvisible ^. gsvsIsDamaged) True
  putStrLn "handle_new_menu"
  return ()

handle_wlr_surface_new_subsurface :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_new_subsurface gsvs args@[wlrSubsurfaceVariant] = do
  putStrLn "handle_wlr_surface_new_subsurface"
  wlrSubsurface <- (fromGodotVariant wlrSubsurfaceVariant :: IO GodotWlrSubsurface) >>= validateSurfaceE
  connectGodotSignal wlrSubsurface "destroy" gsvs "handle_wlr_subsurface_destroy" []
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
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
handle_wlr_subsurface_destroy gsvs args@[wlrSubsurfaceVariant] = do
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  return ()

handle_wlr_surface_commit :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_commit gsvs args@[wlrSurfaceVariant] = do
  return ()

handle_wlr_surface_destroy :: GodotSimulaViewSprite -> [GodotVariant] -> IO ()
handle_wlr_surface_destroy gsvs args@[wlrSurfaceVariant] = do
  atomically $ writeTVar (gsvs ^. gsvsIsDamaged) True
  return ()