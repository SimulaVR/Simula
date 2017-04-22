module Simula.NewCompositor.OpenGL where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import Data.IORef
import Data.FileEmbed
import Data.Typeable
import Linear

import Graphics.Rendering.OpenGL
import Foreign

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


setViewPort :: ViewPort -> IO ()
setViewPort this = do
  offset <- readIORef $ _viewPortOffset this
  size <- readIORef $ _viewPortSize this
  viewport $= (Position (truncate $ offset ^. _x) (truncate (offset ^. _y)), Size (truncate $ size ^. _x) (truncate $ size ^. _y))

class OpenGLContext a where
  glCtxDefaultFramebufferSize :: a -> IO (V2 Int)
  glCtxMakeCurrent :: a -> IO ()


data MotorcarShader
  = ShaderMotorcarLine
  | ShaderMotorcarSurface
  deriving (Show, Eq, Ord, Enum)

vertexSource, fragSource :: MotorcarShader -> ByteString
vertexSource ShaderMotorcarLine =  $(embedFile "shaders/motorcarline.vert")
vertexSource ShaderMotorcarSurface =  $(embedFile "shaders/motorcarsurface.vert")
fragSource ShaderMotorcarLine = $(embedFile "shaders/motorcarline.frag")
fragSource ShaderMotorcarSurface =  $(embedFile "shaders/motorcarsurface.frag")

getProgram :: MotorcarShader -> IO Program
getProgram shader = do
  vert <- createShader VertexShader
  frag <- createShader FragmentShader
  shaderSourceBS vert $= vertexSource shader
  shaderSourceBS frag $= fragSource shader
  compileShader vert
  compileShader frag

  program <- createProgram
  attachShader program vert
  attachShader program frag
  linkProgram program

  return program
