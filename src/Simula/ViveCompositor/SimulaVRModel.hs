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
--  , _simulaVRModelPointerRay :: SimulaVRModelRay
  } deriving Eq

data SimulaVRModelRay = SimulaVRModelRay
  { _simulaVRModelRayBase :: BaseDrawable
  , _simulaVRModelRayVertexBuffer :: BufferObject
  , _simulaVRModelRayVertexArray :: VertexArrayObject
  , _simulaVRModelRayProgram :: Program
  , _simulaVRModelRayMatrixUniform :: UniformLocation
  , _simulaVRModelRayColorUniform :: UniformLocation
  } deriving Eq


makeLenses ''SimulaVRModel
makeLenses ''SimulaVRModelRay

instance HasBaseSceneGraphNode SimulaVRModel where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseSceneGraphNode SimulaVRModelRay where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable SimulaVRModel where
  baseDrawable = simulaVRModelBase

instance HasBaseDrawable SimulaVRModelRay where
  baseDrawable = simulaVRModelRayBase

instance SceneGraphNode SimulaVRModel where
  nodeOnFrameDraw = drawableOnFrameDraw

instance SceneGraphNode SimulaVRModelRay where
  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode SimulaVRModel
instance VirtualNode SimulaVRModelRay

newSimulaVrModel :: SceneGraphNode a => a -> String -> RenderModel -> RenderModel_TextureMap -> IO SimulaVRModel
newSimulaVrModel parent name rm rmTex = do
  program <- getProgram ShaderSimulaVRModel
  currentProgram $= Just program
  aPosition <- get $ attribLocation program "aPosition"
--  aNormal <- get $ attribLocation program "aNormal"
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

--  vertexAttribArray aNormal $= Enabled
--  vertexAttribPointer aNormal $= (ToFloat, VertexArrayDescriptor 3 Float sizeOfRenderModelVertex offsetOfRenderModelVertexNormal)
--  checkForErrors

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

  uMatrix <- get $ uniformLocation program "uMatrix"
  currentProgram $= Nothing
  checkForErrors

  rec
    let model = SimulaVRModel base name vertCount vertBuffer indexBuffer vao tex program uMatrix
    base <- newBaseDrawable model (Just (Some parent)) identity
  newSimulaVRModelRay model
  return model

newSimulaVRModelRay :: SceneGraphNode a => a ->  IO SimulaVRModelRay
newSimulaVRModelRay parent = do
  program <- getProgram ShaderSimulaVRModelRay
  currentProgram $= Just program
  aPosition <- get $ attribLocation program "aPosition"
  checkForErrors

  vao <- genObjectName
  
  bindVertexArrayObject $= Just vao
  checkForErrors
  
  vertBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertBuffer

  
  withArrayLen vertices $ \len ptr -> 
    bufferData ArrayBuffer $= (fromIntegral $ len * sizeOf (undefined :: Float), ptr, StaticDraw)
  checkForErrors

  vertexAttribArray aPosition $= Enabled
  vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 4 Float 0 nullPtr)
  checkForErrors

  uMatrix <- get $ uniformLocation program "uMVPMatrix"
  uColor <- get $ uniformLocation program "uColor"


  bindVertexArrayObject $= Nothing
  currentProgram $= Nothing

  rec
    let ray = SimulaVRModelRay base vertBuffer vao program uMatrix uColor
    base <- newBaseDrawable ray (Just (Some parent)) identity
  return ray

  where
    vertices = [ 0, 0, 0, 1
               , 0, 0,-1, 1
               ] :: [Float]
  

instance Drawable SimulaVRModel where
  drawableDraw model scene display = do
    -- i don't think this is good ogl practice, but eh
    oldVao <- get bindVertexArrayObject
    currentProgram $= Just (model ^. simulaVRModelProgram)
    bindVertexArrayObject $= Just (model ^. simulaVRModelVertexArray)
    textureBinding Texture2D $= Just (model ^. simulaVRModelTexture)
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
  
instance Drawable SimulaVRModelRay where
  drawableDraw ray scene display = do
    depthFunc $= Nothing
    oldVao <- get bindVertexArrayObject
    currentProgram $= Just (ray ^. simulaVRModelRayProgram)
    bindVertexArrayObject $= Just (ray ^. simulaVRModelRayVertexArray)
    checkForErrors

    let uMatrix = ray ^. simulaVRModelRayMatrixUniform
    let uColor = ray ^. simulaVRModelRayColorUniform
    worldTf <- nodeWorldTransform ray

    uniform uColor $= (Color3 1 1 0 :: Color3 Float)
    checkForErrors

    viewpoints <- readMVar $ display ^. displayViewpoints
    forM_ viewpoints $ \vp -> do
      projMatrix <- readMVar (vp ^. viewPointProjectionMatrix)
      viewMatrix <- readMVar (vp ^. viewPointViewMatrix)
      
      let mat = (projMatrix !*! viewMatrix !*! worldTf) ^. m44GLmatrix
      uniform uMatrix $= mat
      checkForErrors


      port <- readMVar (vp ^. viewPointViewPort)
      setViewPort port
      drawArrays Lines 0 2
      checkForErrors
    depthFunc $= Just Less
    bindVertexArrayObject $= oldVao
    currentProgram $= Nothing

