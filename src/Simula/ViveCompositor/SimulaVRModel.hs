module Simula.ViveCompositor.SimulaVRModel where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.Word
import Foreign
import Linear
import Linear.OpenGL
import Graphics.Rendering.OpenGL hiding (scale, Plane, translate)
import OpenVR
import Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.OpenGL
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Utils

-- TODO cache programs
data SimulaVRModel = SimulaVRModel
  { _simulaVRModelBase :: BaseDrawable
  , _simulaVRModelName :: String
  , _simulaVRModelVertexCount :: Int
  , _simulaVRModelVertexBuffer :: BufferObject
  , _simulaVRModelIndexBuffer :: BufferObject
  , _simulaVRModelVertexArray :: VertexArrayObject
  , _simulaVRModelTexture :: TextureObject
  , _simulaVRModelProgram :: Program
  , _simulaVRModelMatrixUniform :: UniformLocation
  } deriving Eq

makeLenses ''SimulaVRModel

instance HasBaseSceneGraphNode SimulaVRModel where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable SimulaVRModel where
  baseDrawable = simulaVRModelBase

instance SceneGraphNode SimulaVRModel where
  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode SimulaVRModel

newSimulaVrModel :: SceneGraphNode a => a -> String -> RenderModel -> RenderModel_TextureMap -> IO SimulaVRModel
newSimulaVrModel parent name rm rmTex = do
  program <- getProgram ShaderSimulaVRModel
  currentProgram $= Just program
  aPosition <- get $ attribLocation program "aPosition"
  aNormal <- get $ attribLocation program "aNormal"
  aTextureCoord <- get $ attribLocation program "aTextureCoord"
  checkForErrors
  
  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  checkForErrors
  
  vertBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertBuffer
  bufferData ArrayBuffer $= (fromIntegral $ modelVertexCount rm * sizeOfRenderModelVertex, modelVertexData rm, StaticDraw)
  checkForErrors

  vertexAttribArray aPosition $= Enabled
  vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float sizeOfRenderModelVertex offsetOfRenderModelVertexPosition)
  checkForErrors

  vertexAttribArray aNormal $= Enabled
  vertexAttribPointer aNormal $= (ToFloat, VertexArrayDescriptor 3 Float sizeOfRenderModelVertex offsetOfRenderModelVertexNormal)
  checkForErrors

  vertexAttribArray aTextureCoord $= Enabled
  vertexAttribPointer aTextureCoord $= (ToFloat, VertexArrayDescriptor 2 Float sizeOfRenderModelVertex offsetOfRenderModelVertexTextureCoord)
  checkForErrors

  let vertCount = modelTriangleCount rm * 3
  indexBuffer <- genObjectName
  bindBuffer ElementArrayBuffer $= Just indexBuffer
  bufferData ElementArrayBuffer $= (fromIntegral $ vertCount * (sizeOf (undefined :: Word16)), modelIndexData rm, StaticDraw)
  checkForErrors

  bindVertexArrayObject $= Nothing

  tex <- genObjectName
  textureBinding Texture2D $= Just tex
  checkForErrors

  let size = TextureSize2D (fromIntegral $ textureMapWidth rmTex) (fromIntegral $ textureMapHeight rmTex)
  texImage2D Texture2D NoProxy 0 RGBA8 size 0 (PixelData RGBA UnsignedByte $ textureMapData rmTex)
  checkForErrors
  
  generateMipmap' Texture2D
  checkForErrors
  
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureFilter Texture2D $= ( (Linear', Just Linear'), Linear' )
  checkForErrors

  get maxTextureMaxAnisotropy >>= (textureMaxAnisotropy Texture2D $=)
  checkForErrors

  textureBinding Texture2D $= Nothing

  uMatrix <- uniformLocation program "uMatrix"
  currentProgram $= Nothing
  checkForErrors

  rec
    let model = SimulaVRModel base name vertCount vertBuffer indexBuffer vao tex program uMatrix
    base <- newBaseDrawable model (Just (Some parent)) identity
  return model


instance Drawable SimulaVRModel where
  drawableDraw model scene display = do
    -- i don't think this is good ogl practice, but eh
    oldVao <- get bindVertexArrayObject
    currentProgram $= Just (model ^. simulaVRModelProgram)
    bindVertexArrayObject $= Just (model ^. simulaVRModelVertexArray)
    textureBinding Texture2D $= Just (model ^. simulaVRModelTexture)
    depthFunc $= Just Less
    checkForErrors
  
    let uMatrix = model ^. simulaVRModelMatrixUniform
    worldTf <- nodeWorldTransform model

    viewpoints <- readMVar $ display ^. displayViewpoints
    forM_ viewpoints $ \vp -> do
      projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
      viewMatrix <- readMVar (vp ^. viewPointViewMatrix)
      
      let mat = (projMatrix !*! viewMatrix !*! worldTf) ^. m44GLmatrix
      uniform uMatrix $= mat
      checkForErrors

      port <- readMVar (vp ^. viewPointViewPort)
      setViewPort port
      drawElements Triangles (fromIntegral $ model ^. simulaVRModelVertexCount) UnsignedShort nullPtr
      checkForErrors
      
    bindVertexArrayObject $= oldVao
    textureBinding Texture2D $= Nothing
    currentProgram $= Nothing

  
