module Simula.Compositor.Compositor where

import Control.Concurrent.MVar
import Foreign

import Simula.WaylandServer

import {-# SOURCE #-} Simula.Compositor.SceneGraph
import Simula.Compositor.OpenGL
import Simula.Compositor.Types
import Simula.Compositor.Wayland.Input
import Simula.Compositor.Wayland.Output


data CompositorType = QtWayland | OsvrQtWayland

class Compositor a where
  startCompositor :: a -> IO ()

  compositorOpenGLContext :: a -> IO (Some OpenGLContext)
  compositorSeat :: a -> IO (Some Seat)

  compositorDisplay :: a -> IO Display

  compositorWlDisplay :: a -> WlDisplay

  compositorGetSurfaceFromResource :: a -> WlResource -> IO (Some WaylandSurface)
