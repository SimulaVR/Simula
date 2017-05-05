module Simula.NewCompositor.OpenGL where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.ByteString (ByteString)
import Data.IORef
import Data.FileEmbed
import Data.Typeable
import Linear

import GHC.Stack
import Graphics.Rendering.OpenGL
import Foreign

import Simula.NewCompositor.Geometry
import Simula.NewCompositor.Types

data ViewPort = ViewPort {
  _viewPortOffset :: IORef (V2 Float),
  _viewPortSize :: IORef (V2 Float),
  _viewPortBufferGeometry :: IORef Rectangle
  } deriving (Eq, Typeable)

makeLenses ''ViewPort

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

class (Eq a, Typeable a) => OpenGLContext a where
  glCtxDefaultFramebufferSize :: a -> IO (V2 Int)
  glCtxMakeCurrent :: a -> IO ()

instance Eq (Some OpenGLContext) where
  Some a == Some b = case cast a of
    Just a -> a == b
    _ -> False

data MotorcarShader
  = ShaderMotorcarLine
  | ShaderMotorcarSurface
  | ShaderDepthCompositedSurface
  | ShaderDepthCompositedSurfaceBlitter
  | ShaderTextureBlitter
  deriving (Show, Eq, Ord, Enum)

vertexSource, fragSource :: MotorcarShader -> ByteString
vertexSource ShaderMotorcarLine =  $(embedFile "shaders/motorcarline.vert")
vertexSource ShaderMotorcarSurface =  $(embedFile "shaders/motorcarsurface.vert")
vertexSource ShaderDepthCompositedSurface = $(embedFile "shaders/depthcompositedsurface.vert")
vertexSource ShaderDepthCompositedSurfaceBlitter = $(embedFile "shaders/depthcompositedsurfaceblitter.vert")
vertexSource ShaderTextureBlitter = $(embedFile "shaders/textureblitter.vert")

fragSource ShaderMotorcarLine = $(embedFile "shaders/motorcarline.frag")
fragSource ShaderMotorcarSurface =  $(embedFile "shaders/motorcarsurface.frag")
fragSource ShaderDepthCompositedSurface = $(embedFile "shaders/depthcompositedsurface.frag")
fragSource ShaderDepthCompositedSurfaceBlitter = $(embedFile "shaders/depthcompositedsurfaceblitter.frag")
fragSource ShaderTextureBlitter = $(embedFile "shaders/textureblitter.frag")

getProgram :: MotorcarShader -> IO Program
getProgram shader = do
  vert <- createShader VertexShader
  frag <- createShader FragmentShader
  shaderSourceBS vert $= vertexSource shader
  shaderSourceBS frag $= fragSource shader
  compileShader vert
  checkCompileStatus vert
  compileShader frag
  checkCompileStatus frag
  
  program <- createProgram
  attachShader program vert
  attachShader program frag
  linkProgram program
  isLinked <- get $ linkStatus program
  when (not isLinked) $ do
    get (programInfoLog program) >>= putStrLn
    ioError . userError $ "Failed linking " ++ show shader


  return program

  where
    checkCompileStatus shader = do
      isCompiled <- get $ compileStatus shader
      when (not isCompiled) $ do
        get (shaderInfoLog shader) >>= putStrLn
        ioError . userError $ "Failed compiling shader " ++ show shader


checkForErrors :: HasCallStack => IO ()
checkForErrors = do
  errs <- get errors
  when (not (null errs)) $ do
    fbStatus <- get $ framebufferStatus Framebuffer
    dfbStatus <- get $ framebufferStatus DrawFramebuffer
    rfbStatus <- get $ framebufferStatus ReadFramebuffer
    putStrLn $ "Framebuffer status: " ++ show fbStatus
    putStrLn $ "Draw framebuffer status: " ++ show dfbStatus
    putStrLn $ "Read framebuffer status: " ++ show rfbStatus
    error $ show errs
