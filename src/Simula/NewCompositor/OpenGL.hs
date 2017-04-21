module Simula.NewCompositor.OpenGL where

import Control.Applicative
import Control.Lens
import Data.IORef
import Data.Typeable
import Linear


import Simula.NewCompositor.Geometry

data ViewPort = ViewPort {
  _viewPortBufferGeometry :: IORef Rectangle,
  _viewPortOffset :: IORef (V2 Float),
  _viewPortSize :: IORef (V2 Float)
  } deriving (Eq, Typeable)

viewPortWidth, viewPortHeight :: ViewPort -> IO Float
viewPortWidth this = view _x <$> readIORef (_viewPortSize this) 
viewPortHeight this = view _y <$> readIORef (_viewPortSize this)

viewPortDisplayCoordsToViewportCoords :: ViewPort -> V2 Float -> IO (V2 Float)
viewPortDisplayCoordsToViewportCoords this pixel = do
  offset <- readIORef $ _viewPortOffset this
  size <- readIORef $ _viewPortSize this
  let res = liftA3 (\p o s -> (p - o)/s - 0.5) pixel offset size
  return $ liftA2 (*) res (V2 1 ((size ^. _y) / (size ^. _x)))


class OpenGLContext a where
  glCtxDefaultFramebufferSize :: a -> IO (V2 Int)
  glCtxMakeCurrent :: a -> IO ()
