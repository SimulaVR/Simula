{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin.Weston (GodotWestonCompositor(..)) where

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

import Plugin.Input
import Plugin.WestonSurfaceSprite
import Plugin.WestonSurfaceTexture

import Control.Monad
import Control.Concurrent
import System.Environment
import Data.Maybe

import System.Process

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
  , _gwcFocus :: TVar (Maybe Focus)
  }

instance GodotClass GodotWestonCompositor where
  godotClassName = "WestonCompositor"

instance ClassExport GodotWestonCompositor where
  classInit obj  = GodotWestonCompositor obj
    <$> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar mempty)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar Nothing)

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" ready
    , GodotMethod NoRPC "_input" input
    ]

instance HasBaseClass GodotWestonCompositor where
  type BaseClass GodotWestonCompositor = GodotSpatial
  super (GodotWestonCompositor obj _ _ _ _ _ _) = GodotSpatial obj

-- deriving instance Show WestonView

ready :: GFunc GodotWestonCompositor
ready compositor _ = do
  startBaseCompositor compositor
  toLowLevel VariantNil

startBaseCompositor :: GodotWestonCompositor -> IO ()
startBaseCompositor compositor = do
  startBaseThread compositor
  startTelemetry (_gwcSurfaces compositor)

startBaseThread :: GodotWestonCompositor -> IO ()
startBaseThread compositor = void $ forkOS $ do
  prevDisplay <- getEnv "DISPLAY"

  wldp  <- wl_display_create
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
  outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi)
  addListenerToSignal outputPendingSignal outputPendingPtr

  let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  outputCreatedPtr <- createNotifyFuncPtr onOutputCreated
  addListenerToSignal outputCreatedSignal outputCreatedPtr

  --createFlushDamageFunc (onFlushDamage compositor) >>= setFlushDamageFunc wcomp

  westonWindowedOutputCreate windowedApi wcomp "Godot"

  output <- atomically $ readTVar (_gwcOutput compositor)

  forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

  let api = defaultWestonDesktopApi
        { apiSurfaceAdded   = onSurfaceCreated
        , apiSurfaceRemoved = onSurfaceDestroyed
        , apiCommitted      = onSurfaceCommit
        }


  westonDesktopCreate wcomp api nullPtr

  seat <- newSeat wcomp "Godot"
  weston_seat_init_pointer seat
  weston_seat_init_keyboard seat (XkbKeymap nullPtr)

  --installHandler sigUSR1 Ignore Nothing
  wet_load_xwayland wcomp

  -- Needs to be set to the original X display rather than
  -- the new one for some reason, or it will crash.
  setEnv "DISPLAY" prevDisplay

  weston_compositor_wake wcomp
  putStrLn "starting compositor"

  -- weston-terminal will be our "launcher" until a real launcher is implemented.
  -- HACK: Sleeping for 3 seconds avoids an extant bug that happens when launching an application too 
  --       soon after a Simula starts
  -- TODO: Create a generic queue for running commands using idle callback
  wlDisplayAddIdleCallback wldp nullPtr (\_ -> callCommand "sleep 3 && weston-terminal &")

  wl_display_run wldp

  where
    onOutputPending windowedApi _ outputPtr = do
      putStrLn "output pending"
      let output = WestonOutput $ castPtr outputPtr
      weston_output_set_scale output 1
      weston_output_set_transform output 0
      westonWindowedOutputSetSize windowedApi output 1280 720
      weston_output_enable output
      return ()

    onOutputCreated _ outputPtr = do
      putStrLn "output created"
      let output = WestonOutput $ castPtr outputPtr
      atomically $ writeTVar (_gwcOutput compositor) output

    onSurfaceCreated desktopSurface  _ = do
      putStrLn "onSurfaceCreated"

      surface <- weston_desktop_surface_get_surface desktopSurface
      view'   <- weston_desktop_surface_create_view desktopSurface
      output  <- atomically $ readTVar (_gwcOutput compositor)
      layer   <- atomically $ readTVar (_gwcNormalLayer compositor)
      westonViewSetOutput view' output
      weston_layer_entry_insert (westonLayerViewList layer) (westonViewLayerEntry view')

      gwst   <- newGodotWestonSurfaceTexture
      setWestonSurface gwst surface view'
      sprite <- newGodotWestonSurfaceSprite gwst =<< getSeat compositor
      G.add_child compositor (safeCast sprite) True
      atomically $ modifyTVar' (_gwcSurfaces compositor) (M.insert surface sprite)

      putStrLn "onSurfaceCreated end"
      return ()

    onSurfaceDestroyed desktopSurface _ = do
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

    onSurfaceCommit desktopSurface _ _ _ = do
      surface     <- weston_desktop_surface_get_surface desktopSurface
      Just sprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))

      updateWestonSurfaceSprite sprite

      -- Clear the pointer's focus if needed
      maybeCurrentActiveFocus <- atomically $ readTVar $ _gwcFocus compositor
      maybeSpriteFocus <- atomically $ readTVar $ _gwssFocused sprite
      seat <- atomically $ readTVar (_gwssSeat sprite)
      pointer <- weston_seat_get_pointer seat
      -- PROBLEM: Running Simula yields 0's, 1's, and 3's printed to console,
      -- but no 2's.
      print "0"
      if (isNothing maybeCurrentActiveFocus) && (isJust maybeSpriteFocus)
        then atomically $ writeTVar (_gwcFocus compositor) maybeSpriteFocus
        else return ()
      if (isJust maybeCurrentActiveFocus && isJust maybeSpriteFocus)
          then do
              print "1"
              let (Just currentActiveFocus) = maybeCurrentActiveFocus
              let (Just spriteFocus) = maybeSpriteFocus
              -- Perhaps pointer comparison (of WestonView) is flawed?
              print $ "spriteFocus view: " ++ (show (_focusView spriteFocus))
              print $ "currentActiveFocus view: "  ++ (show (_focusView currentActiveFocus))
              if ((_focusTimeSpec spriteFocus) > (_focusTimeSpec currentActiveFocus)) && ((_focusView spriteFocus) /= (_focusView currentActiveFocus)) -- > inverted
                then do weston_pointer_clear_focus pointer
                        atomically $ writeTVar (_gwcFocus compositor) (Just spriteFocus)
                        print "2"
                else do -- atomically $ writeTVar (_gwssFocused sprite) Nothing
                        print "3"
           else return ()

      whenM (spriteShouldMove sprite) $ do
        setSpriteShouldMove sprite False
        moveToUnoccupied compositor sprite

-- TODO: check the origin plane?
moveToUnoccupied :: GodotWestonCompositor -> GodotWestonSurfaceSprite -> IO ()
moveToUnoccupied gwc gwss = do
  surfaces <- atomically $ readTVar (_gwcSurfaces gwc)
  let elems = filter (\x -> asObj x /= asObj gwss) $ M.elems surfaces

  extents <- forM elems $ \westonSprite -> do
    sprite <- getSprite westonSprite
    aabb   <- G.get_transformed_aabb sprite
    size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
    pos    <- Api.godot_aabb_get_position aabb >>= fromLowLevel
    return (pos, size + pos)

  let minX = minimum $ 0 : map (view $ _1._x) extents
      maxX = maximum $ 0 :  map (view $ _2._x) extents
  sprite <- getSprite gwss
  aabb   <- G.get_aabb sprite
  size   <- Api.godot_aabb_get_size aabb >>= fromLowLevel
  let sizeX  = size ^. _x
      newPos =
        if abs minX < abs maxX
        then V3 (minX - sizeX/2) 0 0
        else V3 (maxX + sizeX/2) 0 0

  G.translate gwss =<< toLowLevel newPos

getSeat :: GodotWestonCompositor -> IO WestonSeat
getSeat gwc = do
  (seat:_) <- atomically (readTVar (_gwcCompositor gwc)) >>= westonCompositorSeats
  return seat


input :: GFunc GodotWestonCompositor
input self args = do
  (getArg' 0 args :: IO GodotObject)
    >>= asClass GodotInputEventKey "InputEventKey" >>= \case
      Just evk -> do
        dsp <- readTVarIO (_gwcWlDisplay self)
        kbd <- getKeyboard self
        processKeyEvent dsp kbd evk
        setInputHandled self

      Nothing  -> return () -- not a key
  toLowLevel VariantNil
  where
    getKeyboard :: GodotWestonCompositor -> IO WestonKeyboard
    getKeyboard gwc = getSeat gwc >>= weston_seat_get_keyboard
