module Simula.BaseCompositor.Compositor where

import Control.Concurrent.MVar
import Foreign

import Simula.WaylandServer

import {-# SOURCE #-} Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.OpenGL
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Wayland.Output

class Compositor a where
  -- data SimulaSurface a -- data family pattern
  startCompositor :: a -> IO ()

  compositorOpenGLContext :: a -> IO (Some OpenGLContext)
  compositorSeat :: a -> IO (Some Seat)

  compositorDisplay :: a -> IO Display

  compositorWlDisplay :: a -> WlDisplay

  compositorGetSurfaceFromResource :: a -> WlResource -> IO (Some WaylandSurface)
