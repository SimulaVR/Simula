module Simula.NewCompositor.SceneGraph.Wayland where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Int
import Data.Typeable
import Data.Word
import Linear
import Linear.OpenGL

import Graphics.Rendering.OpenGL
import Foreign

import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types


data OpenGLShader


class VirtualNode a => Drawable a where
  drawableDraw :: a -> Scene -> Display -> IO ()
  drawableVisible :: a -> IO Bool
  setDrawableVisible :: a -> Bool -> IO ()

drawableOnFrameDraw :: Drawable a => a -> Maybe Scene -> IO ()
drawableOnFrameDraw this (Just scene) = do
  visible <- drawableVisible this
  -- display is always activated beforehand
  Just display <- readIORef $ _sceneActiveDisplay scene
  when visible $ drawableDraw this scene display
drawableOnFrameDraw _ _ = return ()

data BaseDrawable = BaseDrawable {
  _baseDrawableBase :: BaseSceneGraphNode,
  _baseDrawableVisible :: IORef Bool
  } deriving (Eq, Typeable)

newBaseDrawable :: Maybe (Some SceneGraphNode) -> M44 Float -> IO BaseDrawable
newBaseDrawable parent tf = BaseDrawable <$> newBaseNode parent tf <*> newIORef True

baseDrawableVisible :: BaseDrawable -> IO Bool
baseDrawableVisible = readIORef . _baseDrawableVisible

setBaseDrawableVisible :: BaseDrawable -> Bool -> IO ()
setBaseDrawableVisible = writeIORef . _baseDrawableVisible

data WireframeNode = WireframeNode {
  _wireFrameNodeBase :: BaseDrawable,
  _wireFrameNodeLineColor :: IORef (Color3 Float),
  _wireFrameNodeSegments :: ForeignPtr Float,
  _wireFrameNodeNumSegments :: Int,
  _wireFrameNodeLineShader :: Program,
  _wireFrameNodeLineVertexCoordinates :: BufferObject,
  _wireFrameNodeAPositionLine :: AttribLocation,
  _wireFrameNodeUMVPMatrixLine, _wireFrameNodeUColorLine :: UniformLocation
  } deriving (Eq, Typeable)

instance SceneGraphNode WireframeNode where
  nodeParent = baseNodeParent . _baseDrawableBase . _wireFrameNodeBase
  setNodeParent' = setBaseNodeParent . _baseDrawableBase . _wireFrameNodeBase
  nodeTransform = baseNodeTransform . _baseDrawableBase . _wireFrameNodeBase
  setNodeTransform' = setBaseNodeTransform . _baseDrawableBase . _wireFrameNodeBase
  nodeChildren = _graphNodeChildren . _baseDrawableBase . _wireFrameNodeBase

  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode WireframeNode

instance Drawable WireframeNode where
  drawableVisible = baseDrawableVisible . _wireFrameNodeBase
  setDrawableVisible = setBaseDrawableVisible . _wireFrameNodeBase

  drawableDraw this scene display = withForeignPtr (_wireFrameNodeSegments this) $ \segPtr -> do
    currentProgram $= Just (_wireFrameNodeLineShader this)
    vertexAttribArray (_wireFrameNodeAPositionLine this) $= Enabled
    bindBuffer ArrayBuffer $= Just (_wireFrameNodeLineVertexCoordinates this)
    vertexAttribPointer (_wireFrameNodeAPositionLine this) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
    bufferData ArrayBuffer $= (fromIntegral $ _wireFrameNodeNumSegments this * 6 * sizeOf (undefined :: Float), segPtr, DynamicDraw)

    lineColor <- readIORef $ _wireFrameNodeLineColor this
    
    uniform (_wireFrameNodeUColorLine this) $= lineColor

    viewpoints <- readIORef $ _displayViewpoints display
    forM_ viewpoints $ \vp -> do
      projMatrix <- readIORef $ _viewPointProjectionMatrix vp
      viewMatrix <- readIORef $ _viewPointViewMatrix vp
      worldTf <- nodeWorldTransform this
      
      let mat = (projMatrix !*! viewMatrix !*! worldTf) ^. m44GLmatrix
      uniform (_wireFrameNodeUMVPMatrixLine this) $= mat

      port <- readIORef $ _viewPointViewPort vp
      setViewPort port

      drawArrays Lines 0 (fromIntegral $ 2 * _wireFrameNodeNumSegments this)

    vertexAttribArray (_wireFrameNodeAPositionLine this) $= Disabled
    currentProgram $= Nothing

newWireframeNode :: SceneGraphNode a => [Float] -> Color3 Float -> a -> M44 Float -> IO WireframeNode
newWireframeNode segs lineColor parent transform = do
  drawable <- newBaseDrawable (Just (Some parent)) transform
  lineColorRef <- newIORef lineColor
  segArray <- newArray segs >>= newForeignPtr finalizerFree
  let numSegments = length segs `div` 6
  coordsBuffer <- genObjectName
  prog <- getProgram ShaderMotorcarLine
  aPos <- get $ attribLocation prog "aPosition"
  uMVP <- get $ uniformLocation prog "uMVPMatrix"
  uColor <- get $ uniformLocation prog "uColor"
  return $ WireframeNode drawable lineColorRef segArray numSegments prog coordsBuffer aPos uMVP uColor
  
