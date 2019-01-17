{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase #-}
module Plugin.Wlroots (GodotWlrootsCompositor(..)) where

import Plugin.WaylandTypes
import Foreign

import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods               as G
import           Godot.Nativescript
import           Godot.Extra.Register

import qualified Data.Map.Strict as M

import Plugin.Input
import Plugin.WlrootsSurfaceSprite
import Plugin.WlrootsSurfaceTexture

import Control.Monad
import Control.Concurrent
import System.Environment

import System.Process

import Control.Lens

import Foreign hiding (void)

import Telemetry


data GodotWlrootsCompositor = GodotWlrootsCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar (Ptr C'WlrCompositor)
  , _gwcWlDisplay :: TVar (Ptr C'WlDisplay)
  , _gwcSurfaces :: TVar (M.Map (Ptr C'WlrSurface) GodotWlrootsSurfaceSprite)
  , _gwcOutput :: TVar (Ptr C'WlrOutput)
  , _gwcNormalLayer :: TVar (Ptr C'WlrLayer)
  }

instance GodotClass GodotWlrootsCompositor where
  godotClassName = "WlrootsCompositor"

instance ClassExport GodotWlrootsCompositor where
  classInit obj  = GodotWlrootsCompositor obj
    <$> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar mempty)
    <*> atomically (newTVar undefined)
    <*> atomically (newTVar undefined)

  classExtends = "Spatial"
  classMethods =
    [ GodotMethod NoRPC "_ready" ready
    , GodotMethod NoRPC "_input" input
    ]

instance HasBaseClass GodotWlrootsCompositor where
  type BaseClass GodotWlrootsCompositor = GodotSpatial
  super (GodotWlrootsCompositor obj _ _ _ _ _) = GodotSpatial obj


ready :: GFunc GodotWlrootsCompositor
ready compositor _ = do
  startBaseCompositor compositor
  toLowLevel VariantNil

startBaseCompositor :: GodotWlrootsCompositor -> IO ()
startBaseCompositor compositor = do
  startBaseThread compositor
  startTelemetry (_gwcSurfaces compositor)

startBaseThread :: GodotWlrootsCompositor -> IO ()
startBaseThread compositor = Control.Monad.void $ forkOS $ do
  putStrLn "startWlrBaseThread has not been implemented yet."
  -- prevDisplay <- getEnv "DISPLAY"

  -- wldp  <- wl_display_create
  -- wcomp <- weston_compositor_create wldp nullPtr
  -- atomically $ writeTVar (_gwcCompositor compositor) wcomp
  -- atomically $ writeTVar (_gwcWlDisplay compositor) wldp
  -- westonCompositorSetRepaintMsec wcomp 1000

  -- setup_weston_log_handler
  -- westonCompositorSetEmptyRuleNames wcomp

  -- --todo hack; make this into a proper withXXX function
  -- res <- with (WlrootsHeadlessBackendConfig (WlrootsBackendConfig westonHeadlessBackendConfigVersion (sizeOf (undefined :: WlrootsHeadlessBackendConfig)))
  --          False) $ weston_compositor_load_backend wcomp WlrootsBackendHeadless . castPtr

  -- when (res > 0) $ ioError $ userError "Error when loading backend"

  -- socketName <- wl_display_add_socket_auto wldp
  -- putStrLn $ "Socket: " ++ socketName
  -- setEnv "WAYLAND_DISPLAY" socketName

  -- mainLayer <- newWlrootsLayer wcomp
  -- weston_layer_set_position mainLayer WlrootsLayerPositionNormal

  -- atomically $ writeTVar (_gwcNormalLayer compositor) mainLayer


  -- windowedApi <- weston_windowed_output_get_api wcomp

  -- let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
  -- outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi)
  -- addListenerToSignal outputPendingSignal outputPendingPtr

  -- let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  -- outputCreatedPtr <- createNotifyFuncPtr onOutputCreated
  -- addListenerToSignal outputCreatedSignal outputCreatedPtr

  -- --createFlushDamageFunc (onFlushDamage compositor) >>= setFlushDamageFunc wcomp

  -- westonWindowedOutputCreate windowedApi wcomp "Godot"

  -- output <- atomically $ readTVar (_gwcOutput compositor)

  -- forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000

  -- let api = defaultWlrootsDesktopApi
  --       { apiSurfaceAdded   = onSurfaceCreated
  --       , apiSurfaceRemoved = onSurfaceDestroyed
  --       , apiCommitted      = onSurfaceCommit
  --       }


  -- westonDesktopCreate wcomp api nullPtr

  -- seat <- newSeat wcomp "Godot"
  -- weston_seat_init_pointer seat
  -- weston_seat_init_keyboard seat (XkbKeymap nullPtr)

  -- --installHandler sigUSR1 Ignore Nothing
  -- wet_load_xwayland wcomp

  -- -- Needs to be set to the original X display rather than
  -- -- the new one for some reason, or it will crash.
  -- setEnv "DISPLAY" prevDisplay

  -- weston_compositor_wake wcomp
  -- putStrLn "starting compositor"

  -- -- weston-terminal will be our "launcher" until a real launcher is implemented.
  -- -- HACK: Sleeping for 3 seconds avoids an extant bug that happens when launching an application too 
  -- --       soon after a Simula starts
  -- -- TODO: Create a generic queue for running commands using idle callback
  -- wlDisplayAddIdleCallback wldp nullPtr (\_ -> callCommand "sleep 3 && weston-terminal &")

  -- wl_display_run wldp

  -- where
  --   onOutputPending windowedApi _ outputPtr = do
  --     putStrLn "output pending"
  --     let output = WlrootsOutput $ castPtr outputPtr
  --     weston_output_set_scale output 1
  --     weston_output_set_transform output 0
  --     westonWindowedOutputSetSize windowedApi output 1280 720
  --     weston_output_enable output
  --     return ()

  --   onOutputCreated _ outputPtr = do
  --     putStrLn "output created"
  --     let output = WlrootsOutput $ castPtr outputPtr
  --     atomically $ writeTVar (_gwcOutput compositor) output

  --   onSurfaceCreated desktopSurface  _ = do
  --     putStrLn "onSurfaceCreated"

  --     surface <- weston_desktop_surface_get_surface desktopSurface
  --     view'   <- weston_desktop_surface_create_view desktopSurface
  --     output  <- atomically $ readTVar (_gwcOutput compositor)
  --     layer   <- atomically $ readTVar (_gwcNormalLayer compositor)
  --     westonViewSetOutput view' output
  --     weston_layer_entry_insert (westonLayerViewList layer) (westonViewLayerEntry view')

  --     gwst   <- newGodotWlrootsSurfaceTexture
  --     setWlrootsSurface gwst surface view'
  --     sprite <- newGodotWlrootsSurfaceSprite gwst =<< getSeat compositor
  --     G.add_child compositor (safeCast sprite) True
  --     atomically $ modifyTVar' (_gwcSurfaces compositor) (M.insert surface sprite)

  --     putStrLn "onSurfaceCreated end"
  --     return ()

  --   onSurfaceDestroyed desktopSurface _ = do
  --     putStrLn "onSurfaceDestroyed"
  --     surface <- weston_desktop_surface_get_surface desktopSurface
  --     maybeSprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
  --     case maybeSprite of
  --       Just sprite -> do
  --         Api.godot_object_destroy (safeCast sprite)
  --         atomically $ modifyTVar' (_gwcSurfaces compositor) (M.delete surface)
  --       _ -> return ()
  --     putStrLn "onSurfaceDestroyed end"
  --     return ()

  --   onSurfaceCommit desktopSurface _ _ _ = do
  --     surface     <- weston_desktop_surface_get_surface desktopSurface
  --     Just sprite <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))

  --     updateWlrootsSurfaceSprite sprite

  --     whenM (spriteShouldMove sprite) $ do
  --       setSpriteShouldMove sprite False
  --       moveToUnoccupied compositor sprite

-- TODO: check the origin plane?
moveToUnoccupied :: GodotWlrootsCompositor -> GodotWlrootsSurfaceSprite -> IO ()
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

getSeat :: GodotWlrootsCompositor -> IO (Ptr C'WlrSeat)
getSeat gwc = undefined
  -- do (seat:_) <- atomically (readTVar (_gwcCompositor gwc)) >>= westonCompositorSeats
  --    return seat


input :: GFunc GodotWlrootsCompositor
input self args = do
  (getArg' 0 args :: IO GodotObject)
    >>= asClass GodotInputEventKey "InputEventKey" >>= \case
      Just evk -> do
        -- dsp <- readTVarIO (_gwcWlDisplay self)
        -- kbd <- getKeyboard self
        -- processKeyEvent dsp kbd evk
        putStrLn "WlrInput handling not yet implemented."
        setInputHandled self

      Nothing  -> return () -- not a key
  toLowLevel VariantNil
  -- where
    -- getKeyboard :: GodotWlrootsCompositor -> IO (Ptr C'WlrKeyboard)
    -- getKeyboard gwc = getSeat gwc >>= weston_seat_get_keyboard
