{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Plugin.Weston where

import Simula.WaylandServer
import Simula.Weston
import Simula.WestonDesktop

import Data.Coerce

import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports

import qualified Godot.Gdnative.Internal.Api as Api
import           Godot.Gdnative.Types        (GodotFFI, LibType, TypeOf)
import qualified Godot.Methods               as G

import qualified Data.Map.Strict as M
import Plugin.WestonSurfaceTexture

import Control.Monad
import Control.Concurrent
import System.Environment

import System.Posix.Signals

import Foreign hiding (void)

data GodotWestonCompositor = GodotWestonCompositor
  { _gwcObj      :: GodotObject
  , _gwcCompositor :: TVar WestonCompositor
  , _gwcSurfaces :: TVar (M.Map WestonSurface (GodotWestonSurfaceTexture, GodotSprite3D))
  , _gwcOutput :: TVar WestonOutput
  , _gwcNormalLayer :: TVar WestonLayer
  }

instance GodotClass GodotWestonCompositor where
  godotClassName = "WestonCompositor"

instance ClassExport GodotWestonCompositor where
  classInit obj  = GodotWestonCompositor obj <$> atomically (newTVar undefined) <*> atomically (newTVar mempty) <*> atomically (newTVar undefined)
                   <*> atomically (newTVar undefined)
    
  classExtends = "Node"
  classMethods = [ Func NoRPC "_ready" startBaseCompositor ]

startBaseCompositor :: GodotFunc GodotWestonCompositor 
startBaseCompositor _ compositor _ = (\_ -> installHandler sigUSR1 Ignore Nothing >> toLowLevel VariantNil) =<< (forkOS $ do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr
  atomically $ writeTVar (_gwcCompositor compositor) wcomp
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

  installHandler sigUSR1 Ignore Nothing
  wet_load_xwayland wcomp



  weston_compositor_wake wcomp
  putStrLn "starting compositor"
  wl_display_run wldp)

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

      gws <- newGodotWestonSurfaceTexture
      setWestonSurface gws surface

      sprite <- GodotSprite3D <$> mkClassInstance "Sprite3D"
      G.add_child compositor (safeCast sprite) True
      G.set_centered sprite False

      atomically $ modifyTVar' (_gwcSurfaces compositor) (M.insert surface (gws, sprite))

      putStrLn "onSurfaceCreated end"
      return ()


    onSurfaceDestroyed compositor desktopSurface _ = do
      putStrLn "onSurfaceDestroyed"
      surface <- weston_desktop_surface_get_surface desktopSurface
      Just (gws, sprite) <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      Api.godot_object_destroy (safeCast sprite)
      Api.godot_object_destroy (safeCast gws)
      atomically $ modifyTVar' (_gwcSurfaces compositor) (M.delete surface)
      putStrLn "onSurfaceDestroyed end"
      return ()
  
    onSurfaceCommit compositor desktopSurface x y _ = do
      putStrLn "onSurfaceCommit"
      surface <- weston_desktop_surface_get_surface desktopSurface
      Just (gws, sprite) <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      updateWestonSurfaceTexture gws
      G.set_texture sprite (safeCast gws)
      putStrLn "onSurfaceCommit end"
{-
    onFlushDamage compositor surface = do
      putStrLn "flush"
      Just (gws, _) <- M.lookup surface <$> atomically (readTVar (_gwcSurfaces compositor))
      updateWestonSurface gws
-}
instance HasBaseClass GodotWestonCompositor where
  type BaseClass GodotWestonCompositor = GodotNode         
  super (GodotWestonCompositor obj  _ _ _ _) = GodotNode obj
