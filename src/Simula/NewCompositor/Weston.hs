module Simula.NewCompositor.Weston where

import Data.Word
import Foreign
import Foreign.C
import System.Environment

import Simula.WaylandServer
import Simula.Weston

import Simula.NewCompositor.SceneGraph

data WestonMotorcarCompositor = WestonMotorcarCompositor {
  _westonMotorcarCompositorDisplay :: WlDisplay,
  _westonMotorcarCompositorWestonCompositor :: WestonCompositor
  }

data WestonMotorcarSeat

newWestonCompositor :: Scene -> IO WestonMotorcarCompositor
newWestonCompositor scene = do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr
  with (WestonBackendConfig westonWaylandBackendConfigVersion 0) $ weston_compositor_load_backend wcomp WestonBackendWayland
  
  socketName <- wl_display_add_socket_auto wldp
  setEnv "WAYLAND_DISPLAY" socketName

  -- do all the setup here
