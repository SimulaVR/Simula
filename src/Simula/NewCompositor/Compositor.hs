module Simula.NewCompositor.Compositor where

import Data.IORef
import Foreign

-- mutually recursive, refactor asap
import {-# SOURCE #-} Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.SceneGraph.Output.Display
import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Wayland.Output

data C'wl_display
data C'wl_resource
data OpenGLContext

data CompositorType = QtWayland | OsvrQtWayland

class Compositor a where
  startCompositor :: a -> IO ()

  compositorOpenGLContext :: a -> IO OpenGLContext
  compositorSeat :: a -> IO (Some Seat)

  compositorDisplay :: a -> IO (Some Display)
  setCompositorDisplay :: a -> (Some Display) -> IO ()

  compositorWlDisplay :: a -> Ptr C'wl_display

  compositorGetSurfaceFromResource :: a -> Ptr C'wl_resource -> IO (Some WaylandSurface)
