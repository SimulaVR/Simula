{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
module Plugin.Weston where

import Simula.WaylandServer
import Simula.Weston
import Simula.WestonDesktop

import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript
import           Godot.Extra.Register

import qualified Data.Map.Strict as M
import Plugin.WestonSurfaceSprite
import Plugin.WestonSurfaceTexture
import Plugin.SimulaController
import Plugin.Input
import Plugin.Input.Grab

import Godot.Core.GodotGlobalConstants


import Control.Monad
import Control.Concurrent
import System.Environment

import System.Posix.Signals

import Data.Bits

import Control.Lens

import Foreign hiding (void)

import Telemetry


data GodotWestonCompositor = GodotWestonCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar WestonCompositor
  , _gwcWlDisplay :: TVar WlDisplay
  , _gwcSurfaces :: TVar (M.Map WestonSurface GodotWestonSurfaceSprite)
  , _gwcOutput :: TVar WestonOutput
  , _gwcNormalLayer :: TVar WestonLayer
  , _gwcGrabState :: TVar GrabState
  }

instance GodotClass GodotWestonCompositor where
  godotClassName = "WestonCompositor"

instance ClassExport GodotWestonCompositor where
  classInit obj  = GodotWestonCompositor obj <$> atomically (newTVar undefined) <*> atomically (newTVar undefined) <*> atomically (newTVar mempty) <*> atomically (newTVar undefined)
                   <*> atomically (newTVar undefined) <*> atomically (newTVar NoGrab)

  classExtends = "Spatial"
  classMethods = [ GodotMethod NoRPC "_ready" startBaseCompositor
                 , GodotMethod NoRPC "_input" input
                 , GodotMethod NoRPC "_process" process
                 , GodotMethod NoRPC "on_button_signal" on_button_signal ]

startBaseCompositor :: GFunc GodotWestonCompositor
startBaseCompositor compositor _ = do
  onReady compositor
  startBaseThread compositor
  startTelemetry (_gwcSurfaces compositor)
  toLowLevel VariantNil

onReady :: GodotWestonCompositor -> IO ()
onReady gwc = do
  gwc `getNode` "../ARVROrigin/LeftController" >>= \case
    Just leftCt -> connectController leftCt
    Nothing -> putStrLn "Left controller node doesn't exist."
  gwc `getNode` "../ARVROrigin/RightController" >>= \case
    Just rightCt -> connectController rightCt
    Nothing -> putStrLn "Right controller node doesn't exist."
  where
    connectController ct = do
      btnPressed <- toLowLevel "button_pressed"
      btnReleased <- toLowLevel "button_release"
      btnSignal <- toLowLevel "on_button_signal"

      argsPressed <- Api.godot_array_new
      toLowLevel (toVariant ct) >>= Api.godot_array_append  argsPressed
      toLowLevel (toVariant True) >>= Api.godot_array_append argsPressed

      argsReleased <- Api.godot_array_new
      toLowLevel (toVariant ct) >>= Api.godot_array_append  argsReleased
      toLowLevel (toVariant False) >>= Api.godot_array_append argsReleased

      G.connect ct btnPressed (safeCast gwc) btnSignal argsPressed 0
      G.connect ct btnReleased (safeCast gwc) btnSignal argsReleased 0
      return ()

startBaseThread :: GodotWestonCompositor -> IO ()
startBaseThread compositor = void $ forkOS $ do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr
  atomically $ writeTVar (_gwcCompositor compositor) wcomp
  atomically $ writeTVar (_gwcWlDisplay compositor) wldp
  westonCompositorSetRepaintMsec wcomp 1000

  setup_weston_log_handler
  westonCompositorSetEmptyRuleNames wcomp

  --todo hack; make this into a proper withXXX function
  res <- with (WestonHeadlessBackendConfig (WestonBackendConfig westonHeadlessBackendConfigVersion (sizeOf (undefined :: WestonHeadlessBackendConfig)))
           False) $ weston_compositor_load_backend wcomp WestonBackendHeadless . castPtr

  when (res > 0) $ ioError $ userError "Error when loading backend"

  socketName <- wl_display_add_socket_auto wldp
  putStrLn $ "Socket: " ++ socketName
  setEnv "WAYLAND_DISPLAY" socketName

  mainLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionNormal

  atomically $ writeTVar (_gwcNormalLayer compositor) mainLayer


  windowedApi <- weston_windowed_output_get_api wcomp

  let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
  outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi compositor)
  addListenerToSignal outputPendingSignal outputPendingPtr

  let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  outputCreatedPtr <- createNotifyFuncPtr (onOutputCreated compositor)
  addListenerToSignal outputCreatedSignal outputCreatedPtr

  --createFlushDamageFunc (onFlushDamage compositor) >>= setFlushDamageFunc wcomp

  westonWindowedOutputCreate windowedApi wcomp "Godot"

  output <- atomically $ readTVar (_gwcOutput compositor)

  forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

  let api = defaultWestonDesktopApi {
        apiSurfaceAdded = onSurfaceCreated compositor,
        apiSurfaceRemoved = onSurfaceDestroyed compositor,
        apiCommitted = onSurfaceCommit compositor
        }


  westonDesktopCreate wcomp api nullPtr

  seat <- newSeat wcomp "Godot"
  weston_seat_init_pointer seat
  weston_seat_init_keyboard seat (XkbKeymap nullPtr)

  installHandler sigUSR1 Ignore Nothing
  {-wet_load_xwayland wcomp-}

  weston_compositor_wake wcomp
  putStrLn "starting compositor"
  wl_display_run wldp

  where
    onOutputPending windowedApi compositor _ outputPtr = do
      putStrLn "output pending"
      let output = WestonOutput $ castPtr outputPtr
      weston_output_set_scale output 1
      weston_output_set_transform output 0
      westonWindowedOutputSetSize windowedApi output 1280 720
      weston_output_enable output
      return ()


    onOutputCreated compositor _ outputPtr = do
      putStrLn "output created"
      let output = WestonOutput $ castPtr outputPtr
      atomically $ writeTVar (_gwcOutput compositor) output


    onSurfaceCreated compositor desktopSurface  _ = do
      putStrLn "onSurfaceCreated"
      surface <- weston_desktop_surface_get_surface desktopSurface
      view <- weston_desktop_surface_create_view desktopSurface
      output <- atomically $ readTVar (_gwcOutput compositor)
      westonViewSetOutput view output
      layer <- atomically $ readTVar (_gwcNormalLayer compositor)
      weston_layer_entry_insert  (westonLayerViewList layer) (westonViewLayerEntry view)

      gwst <- newGodotWestonSurfaceTexture

      setWestonSurface gwst surface view

      seat <- getSeat compositor
      sprite <- newGodotWestonSurfaceSprite gwst seat

      G.add_child compositor (safeCast sprite) True

      atomically $ modifyTVar' (_gwcSurfaces compositor) (M.insert surface sprite)

      putStrLn "onSurfaceCreated end"
      return ()


    onSurfaceDestroyed compositor desktopSurface _ = do
      putStrLn "onSurfaceDestroyed"
      surface <- weston_desktop_surface_get_surface desktopSurface
      maybeSprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      case maybeSprite of
        Just sprite -> do
          Api.godot_object_destroy (safeCast sprite)
          atomically $ modifyTVar' (_gwcSurfaces compositor) (M.delete surface)
        _ -> return ()
      putStrLn "onSurfaceDestroyed end"
      return ()

    onSurfaceCommit compositor desktopSurface x y _ = do
      surface <- weston_desktop_surface_get_surface desktopSurface
      Just sprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      updateWestonSurfaceSprite sprite
      move <- spriteShouldMove sprite
      when move $ do
        setSpriteShouldMove sprite False
        moveToUnoccupied compositor sprite

-- TODO: check the origin plane?
moveToUnoccupied :: GodotWestonCompositor -> GodotWestonSurfaceSprite -> IO ()
moveToUnoccupied gwc gwss = do
  surfaces <- atomically $ readTVar (_gwcSurfaces gwc)
  let elems = filter (\x -> asObj x /= asObj gwss) $ M.elems surfaces

  extents <- forM elems $ \westonSprite -> do
    sprite <- getSprite westonSprite
    aabb <- G.get_transformed_aabb sprite
    size <- Api.godot_aabb_get_size aabb >>= fromLowLevel
    pos <- Api.godot_aabb_get_position aabb >>= fromLowLevel

    return (pos, size + pos)

  let minX = minimum $ 0 : map (view $ _1._x) extents
  let maxX = maximum $ 0 :  map (view $ _2._x) extents
  sprite <- getSprite gwss
  aabb <- G.get_aabb sprite
  size <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX = size ^. _x
  let newPos =
        if abs minX < abs maxX
        then V3 (minX - sizeX/2) 0 0
        else V3 (maxX + sizeX/2) 0 0
  tlVec <- toLowLevel newPos
  G.translate gwss tlVec

instance HasBaseClass GodotWestonCompositor where
  type BaseClass GodotWestonCompositor = GodotSpatial
  super (GodotWestonCompositor obj  _ _ _ _ _ _) = GodotSpatial obj

getSeat :: GodotWestonCompositor -> IO WestonSeat
getSeat gwc = do
  (seat:_) <- atomically (readTVar (_gwcCompositor gwc)) >>= westonCompositorSeats
  return seat

input :: GFunc GodotWestonCompositor
input self args = do
  (getArg' 0 args :: IO GodotObject)
    >>= asClass GodotInputEventKey "InputEventKey" >>= \case
      Nothing -> return () -- not a key
      Just evk -> do
        dsp <- readTVarIO (_gwcWlDisplay self)
        kbd <- getKeyboard self
        processKeyEvent dsp kbd evk
        setInputHandled self
  toLowLevel VariantNil
  where
    getKeyboard :: GodotWestonCompositor -> IO WestonKeyboard
    getKeyboard gwc = getSeat gwc >>= weston_seat_get_keyboard

    getPointer :: GodotWestonCompositor -> IO WestonPointer
    getPointer gwc = getSeat gwc >>= weston_seat_get_pointer

on_button_signal :: GFunc GodotWestonCompositor
on_button_signal self args = do
  case toList args of
    [buttonVar, controllerVar, pressedVar] -> do
      button <- fromGodotVariant buttonVar
      controllerObj <- fromGodotVariant controllerVar
      Just controller <- tryObjectCast controllerObj
      pressed <- fromGodotVariant pressedVar
      onButton self controller button pressed
    _ -> return ()
  toLowLevel VariantNil

onButton :: GodotWestonCompositor -> GodotSimulaController -> Int -> Bool -> IO ()
onButton self gsc button pressed = do
  if button == OVR_Button_Grip && not pressed -- Release grabbed
    then
      readTVarIO (_gwcGrabState self)
      >>= processGrabEvent gsc Nothing pressed
      >>= atomically
      .   writeTVar (_gwcGrabState self)
    else do
      whenM (G.is_colliding rc) $ do
        G.get_collider rc >>= tryObjectCast @GodotWestonSurfaceSprite >>= \case
          Just sprite -> onSpriteButton sprite
          Nothing     -> return ()
 where
  rc = _gscRayCast gsc

  onSpriteButton :: GodotWestonSurfaceSprite -> IO ()
  onSpriteButton sprite = G.get_collision_point rc >>= case button of
    OVR_Button_Grip -> \_ ->
      readTVarIO (_gwcGrabState self)
        >>= processGrabEvent gsc (Just sprite) pressed
        >>= atomically
        .   writeTVar (_gwcGrabState self)
    OVR_Button_Trigger -> processClickEvent sprite (Button pressed BUTTON_LEFT)
    OVR_Button_AppMenu -> processClickEvent sprite (Button pressed BUTTON_RIGHT)
    _ -> \_ -> return ()

process :: GFunc GodotWestonCompositor
process self _ = do
  atomically (readTVar (_gwcGrabState self))
    >>= handleState
    >>= atomically . writeTVar (_gwcGrabState self)

  toLowLevel VariantNil
