module Simula.NewCompositor.SceneGraph.Wayland where

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Int
import Data.Typeable
import Data.Word
import Linear
import Linear.OpenGL

import Graphics.Rendering.OpenGL hiding (scale, Plane)
import Foreign

import Simula.NewCompositor.Geometry
import Simula.NewCompositor.Utils
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types
import Simula.NewCompositor.Wayland.Output

data BaseWaylandSurfaceNode = BaseWaylandSurfaceNode {
  _waylandSurfaceNodeBase :: BaseDrawable,
  _waylandSurfaceNodeSurface :: IORef (Some WaylandSurface),
  _waylandSurfaceNodeSurfaceTransform :: IORef (M44 Float),
  _waylandSurfaceNodeDecorations :: WireframeNode,
  _waylandSurfaceNodeTextureCoords :: BufferObject,
  _waylandSurfaceNodeVertexCoords :: BufferObject,
  _waylandSurfaceNodeShader :: Program,
  _waylandSurfaceNodeAPosition, _waylandSurfaceNodeATexCoord :: AttribLocation,
  _waylandSurfaceNodeUMVPMatrix :: UniformLocation
  } deriving (Eq, Typeable)

data MotorcarSurfaceNode = MotorcarSurfaceNode {
  _motorcarSurfaceNodeBase :: BaseWaylandSurfaceNode,

  _motorcarSurfaceNodeDepthCompositedSurfaceShader :: Program,
  _motorcarSurfaceNodeDepthCompositedSurfaceBlitterShader :: Program,
  _motorcarSurfaceNodeClippingShader :: Program,
    
  _motorcarSurfaceNodeColorTextureCoords :: BufferObject,
  _motorcarSurfaceNodeDepthTextureCoords :: BufferObject,
  _motorcarSurfaceNodeSurfaceTextureCoords :: BufferObject,
  _motorcarSurfaceNodeCuboidClippingVertices :: BufferObject,
  _motorcarSurfaceNodeCuboidClippingIndices :: BufferObject,
  
  _motorcarSurfaceNodeAPositionDepthComposite:: AttribLocation,
  _motorcarSurfaceNodeAColorTexCoordDepthComposite:: AttribLocation,
  _motorcarSurfaceNodeADepthCoordDepthComposite:: AttribLocation,

  _motorcarSurfaceNodeAPositionBlit :: AttribLocation,
  _motorcarSurfaceNodeATexCoordBlit :: AttribLocation,
  _motorcarSurfaceNodeUColorSamplerBlit :: UniformLocation,
  _motorcarSurfaceNodeUDepthSamplerBlit :: UniformLocation,

  _motorcarSurfaceNodeAPositionClipping :: AttribLocation,
  _motorcarSurfaceNodeUMVPMatrixClipping :: UniformLocation,
  _motorcarSurfaceNodeUColorClipping :: UniformLocation,

  _motorcarSurfaceNodeDimensions :: V3 Float
  } deriving (Eq, Typeable)



makeClassy ''BaseWaylandSurfaceNode
makeClassy ''MotorcarSurfaceNode

class Drawable a => WaylandSurfaceNode a where
  computeLocalSurfaceIntersection :: a -> Ray -> IO (Maybe (V2 Float, Float))
  default computeLocalSurfaceIntersection :: HasBaseWaylandSurfaceNode a => a -> Ray -> IO (Maybe (V2 Float, Float))
  computeLocalSurfaceIntersection this ray
    | dot (ray ^. rayPos) (surfacePlane ^. planeNorm) == 0
    = return Nothing
  
    | otherwise = do
        tfRay <- (transformRay ray . inv44) <$> readIORef (this ^. waylandSurfaceNodeSurfaceTransform)
        let t = intersectPlane surfacePlane tfRay
        let intersection = solveRay tfRay t
        Some ws <- readIORef (this ^. waylandSurfaceNodeSurface)
        size <- (fmap.fmap) fromIntegral $ wsSize ws
        let coords = liftI2 (*) intersection (V3 (size ^. _x) (size ^. _y) 0)
      
        return $ if t >= 0 then Just (coords ^. _xy, t) else Nothing

    where
      surfacePlane = Plane (V3 0 0 0) (V3 0 0 1)

  computeSurfaceTransform :: a -> Float -> IO ()
  default computeSurfaceTransform :: HasBaseWaylandSurfaceNode a => a -> Float -> IO ()
  computeSurfaceTransform this ppcm = when (ppcm > 0) $ do
    let ppm = 100*ppcm
    let rotQ = axisAngle (V3 0 0 1) pi
    -- TODO test if it's identical
    let rotM = mkTransformation rotQ (V3 (negate 0.5) (negate 0.5) 0)
      
    Some surface <- readIORef (this ^. waylandSurfaceNodeSurface)
    size <- (fmap . fmap) fromIntegral $ wsSize surface
    
    let scaleM = scale identity $ V3 (negate (size ^. _x) / ppm)  ((size ^. _y) / ppm) 1
          
    writeIORef (this ^. waylandSurfaceNodeSurfaceTransform) $ scaleM !*! rotM
    
    setNodeTransform (this ^. waylandSurfaceNodeDecorations) $ scaleM !*! mkTransformation rotQ (V3 0 0 0) !*! scale identity (V3 1.04 1.04 0)

instance HasBaseSceneGraphNode BaseWaylandSurfaceNode where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable BaseWaylandSurfaceNode where
  baseDrawable = waylandSurfaceNodeBase

instance HasBaseSceneGraphNode MotorcarSurfaceNode where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable MotorcarSurfaceNode where
  baseDrawable = baseWaylandSurfaceNode.baseDrawable

instance HasBaseWaylandSurfaceNode MotorcarSurfaceNode where
  baseWaylandSurfaceNode = motorcarSurfaceNodeBase

instance SceneGraphNode BaseWaylandSurfaceNode where
  nodeOnFrameBegin this _ = do
    computeSurfaceTransform this 8
    Some surface <- readIORef $ _waylandSurfaceNodeSurface this
    wsPrepare surface
    
  nodeOnFrameDraw = drawableOnFrameDraw

  nodeIntersectWithSurfaces this ray = do
    closestSubtreeIntersection <- defaultNodeIntersectWithSurfaces this ray
    Some surface <- readIORef $ this ^. waylandSurfaceNodeSurface
    ty <- wsType surface
    case ty of
      Cursor -> return closestSubtreeIntersection
      _ -> do
        tf <- nodeTransform this
        let localRay = transformRay ray (inv44 tf)

        maybeIsec <- computeLocalSurfaceIntersection this localRay

        size <- (fmap . fmap) fromIntegral $ wsSize surface

        case (maybeIsec, closestSubtreeIntersection) of
          (Just (isec, t), Just closest)
            | isec ^. _x >= 0 && isec ^. _y >= 0
              && and (liftI2 (<=) isec size)
              && t < (closest ^. rsiT)
            -> return . Just $ RaySurfaceIntersection (Some this) isec ray t
          _ -> return closestSubtreeIntersection
                             
      

instance VirtualNode BaseWaylandSurfaceNode

instance Drawable BaseWaylandSurfaceNode where
  drawableDraw this scene display = do
    Some surface <- readIORef $ _waylandSurfaceNodeSurface this
    texture <- wsTexture surface

    currentProgram $= Just (_waylandSurfaceNodeShader this)

    let aPosition = _waylandSurfaceNodeAPosition this
    let aTexCoord = _waylandSurfaceNodeATexCoord this
    let uMVPMatrix = _waylandSurfaceNodeUMVPMatrix this
    let vertexCoords = _waylandSurfaceNodeVertexCoords this
    let textureCoords = _waylandSurfaceNodeTextureCoords this
    
    vertexAttribArray aPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vertexCoords
    vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)

    vertexAttribArray aTexCoord $= Enabled
    bindBuffer ArrayBuffer $= Just textureCoords
    vertexAttribPointer aTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

    textureBinding Texture2D $= Just texture
    textureFilter Texture2D $= ((Linear', Nothing), Linear')

    surfaceTf <- readIORef $ _waylandSurfaceNodeSurfaceTransform this
    viewpoints <- readIORef $ _displayViewpoints display
    forM_ viewpoints $ \vp -> do
      --TODO compare w/ order in draw for WireFrameNode 
      port <- readIORef $ _viewPointViewPort vp
      setViewPort port
      
      projMatrix <- readIORef $ _viewPointProjectionMatrix vp
      viewMatrix <- readIORef $ _viewPointViewMatrix vp
      worldTf <- nodeWorldTransform this
    
      let mat = (projMatrix !*! viewMatrix !*! worldTf !*! surfaceTf) ^. m44GLmatrix
      uniform uMVPMatrix $= mat
      drawArrays TriangleFan 0 4

    textureBinding Texture2D $= Nothing
    vertexAttribArray aPosition $= Disabled
    vertexAttribArray aTexCoord $= Disabled
    currentProgram $= Nothing

instance WaylandSurfaceNode BaseWaylandSurfaceNode

instance Drawable MotorcarSurfaceNode where
  drawableDraw this scene display = do
    stencilTest $= Enabled
    scratchFb <- readIORef (display ^. displayScratchFrameBuffer)
    bindFramebuffer DrawFramebuffer $= scratchFb
    clearColor $= Color4 0 0 0 0
    clearDepth $= 1
    clearStencil $= 0
    stencilMask $= 0xff
    clear [ColorBuffer, DepthBuffer, StencilBuffer]

    drawWindowBoundsStencil this display

    {-
    if(surface()->depthCompositingEnabled()){
        glUseProgram(m_depthCompositedSurfaceShader->handle());

        glEnableVertexAttribArray(h_aPosition_depthcomposite);
        glBindBuffer(GL_ARRAY_BUFFER, m_surfaceVertexCoordinates);
        glVertexAttribPointer(h_aPosition_depthcomposite, 3, GL_FLOAT, GL_FALSE, 0, 0);

        glEnableVertexAttribArray(h_aColorTexCoord_depthcomposite);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glEnableVertexAttribArray(h_aDepthTexCoord_depthcomposite);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
    }else{
        glUseProgram(m_surfaceShader->handle());

        glEnableVertexAttribArray(h_aPosition_surface);
        glBindBuffer(GL_ARRAY_BUFFER, m_surfaceVertexCoordinates);
        glVertexAttribPointer(h_aPosition_surface, 3, GL_FLOAT, GL_FALSE, 0, 0);

        glEnableVertexAttribArray(h_aTexCoord_surface);
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        glUniformMatrix4fv(h_uMVPMatrix_surface, 1, GL_FALSE, glm::value_ptr(glm::mat4(1)));

        glDisable(GL_DEPTH_TEST);
        glDepthMask(GL_FALSE);
    }



    GLuint texture = this->surface()->texture();

    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);


    glm::vec4 vp;
    for(ViewPoint *viewpoint : display->viewpoints()){

        viewpoint->viewport()->set();

        if(surface()->depthCompositingEnabled()){
            vp = viewpoint->clientColorViewport()->viewportParams();
            const GLfloat clientColorTextureCoordinates[] = {
                vp.x, 1 - vp.y,
                vp.x + vp.z, 1 - vp.y,
                vp.x + vp.z, 1 - (vp.y + vp.w),
                vp.x, 1 - (vp.y + vp.w),
            };
            glVertexAttribPointer(h_aColorTexCoord_depthcomposite, 2, GL_FLOAT, GL_FALSE, 0, clientColorTextureCoordinates);
            vp = viewpoint->clientDepthViewport()->viewportParams();

            const GLfloat clientDepthTextureCoordinates[] = {
                vp.x, 1 - vp.y,
                vp.x + vp.z, 1 - vp.y,
                vp.x + vp.z, 1 - (vp.y + vp.w),
                vp.x, 1 - (vp.y + vp.w),
            };
            glVertexAttribPointer(h_aDepthTexCoord_depthcomposite, 2, GL_FLOAT, GL_FALSE, 0, clientDepthTextureCoordinates);

        }else {
            vp = viewpoint->viewport()->viewportParams();
            const GLfloat clientColorTextureCoordinates[] = {
                vp.x, 1 - vp.y,
                vp.x + vp.z, 1 - vp.y,
                vp.x + vp.z, 1 - (vp.y + vp.w),
                vp.x, 1 - (vp.y + vp.w),
            };
            glVertexAttribPointer(h_aTexCoord_surface, 2, GL_FLOAT, GL_FALSE, 0, clientColorTextureCoordinates);
        }

        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    }

    if(!this->surface()->depthCompositingEnabled()){
        glEnable(GL_DEPTH_TEST);
        glDepthMask(GL_TRUE);
    }

    clipWindowBounds(display);

    this->drawFrameBufferContents(display);



    glBindFramebuffer(GL_FRAMEBUFFER, display->activeFrameBuffer());

    glDisableVertexAttribArray(h_aPosition_depthcomposite);
    glDisableVertexAttribArray(h_aColorTexCoord_depthcomposite);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, 0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glUseProgram(0);

    glDisable(GL_STENCIL_TEST); -}

    where
      drawWindowBoundsStencil this display = _
      {- void MotorcarSurfaceNode::drawWindowBoundsStencil(Display *display)
{
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    glDepthMask(GL_FALSE);
    glStencilFunc(GL_NEVER, 1, 0xFF);
    glStencilOp(GL_REPLACE, GL_KEEP, GL_KEEP);



    glUseProgram(m_clippingShader->handle());

    glEnableVertexAttribArray(h_aPosition_clipping);
    glBindBuffer(GL_ARRAY_BUFFER, m_cuboidClippingVertices);
    glVertexAttribPointer(h_aPosition_clipping, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glUniform3f(h_uColor_clipping, 1.f, 0.f, 0.f);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_cuboidClippingIndices);

    glm::mat4 modelMatrix = this->worldTransform() * glm::scale(glm::mat4(), this->dimensions());

    int numElements = 36;

    for(ViewPoint *viewpoint : display->viewpoints()){
        viewpoint->viewport()->set();

        glm::mat4 mvp = viewpoint->projectionMatrix() * viewpoint->viewMatrix() * modelMatrix;
        glUniformMatrix4fv(h_uMVPMatrix_clipping, 1, GL_FALSE, glm::value_ptr(mvp));
        glDrawElements(GL_TRIANGLES, numElements,GL_UNSIGNED_INT, 0);
    }

    glDisableVertexAttribArray(h_aPosition_clipping);

    glUseProgram(0);

    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glDepthMask(GL_TRUE);


    glStencilMask(0x00);

    glStencilFunc(GL_EQUAL, 1, 0xFF);
} -}
      

instance WaylandSurfaceNode MotorcarSurfaceNode where
  computeLocalSurfaceIntersection this ray | t >= 0 = return $ Just (V2 0 0, t)
                                           | otherwise = return Nothing
    where
      box = AxisAlignedBox (this ^. motorcarSurfaceNodeDimensions)
      t = intersectBox box ray 0 100
  
  computeSurfaceTransform this ppcm = writeIORef (this ^. baseWaylandSurfaceNode.waylandSurfaceNodeSurfaceTransform) identity

newWaylandSurfaceNode :: (WaylandSurface ws, SceneGraphNode a) => ws -> a -> M44 Float -> IO BaseWaylandSurfaceNode
newWaylandSurfaceNode ws parent tf = do
  program <- getProgram ShaderMotorcarSurface

  texCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just texCoords
  withArrayLen textureCoordinates $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)
  
  verCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just verCoords
  withArrayLen vertexCoordinates $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)

  aPos <- get $ attribLocation program "aPosition"
  aTex <- get $ attribLocation program "aTexCoord"
  uMVP <- get $ uniformLocation program "uMVPMatrix"

  let decoVert = concat [ mkDeco i j k | i <- [-1,1], j <- [-1,1], k <- [-1,1] ]
  let decoColor = Color3 0.5 0.5 0.5

  rec decoNode <- newWireframeNode decoVert decoColor node identity
      node <- BaseWaylandSurfaceNode
              <$> newBaseDrawable (Just (Some parent)) tf
              <*> newIORef (Some ws)
              <*> newIORef identity
              <*> pure decoNode
              <*> pure texCoords
              <*> pure verCoords
              <*> pure program
              <*> pure aPos
              <*> pure aTex
              <*> pure uMVP
  return node

  where
    textureCoordinates :: [Float]
    textureCoordinates = [0, 0, 0, 1, 1, 1, 1, 0]

    vertexCoordinates :: [Float]
    vertexCoordinates = [0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0]

    mkDeco i j k =
      let direction = V3 i j k
          corner = direction ^* 0.5 :: V3 Float
      in concatMap (mkVertices corner direction) [ex, ey, ez]

    mkVertices corner@(V3 cx cy cz) direction (E coord) =
      let V3 sx sy sz = corner & coord -~ (direction ^. coord)
      in [cx, cy, cz, sx, sy, sz]



newMotorcarSurfaceNode :: (WaylandSurface ws, SceneGraphNode a) => ws -> a -> M44 Float -> V3 Float -> IO MotorcarSurfaceNode
newMotorcarSurfaceNode ws prt tf dims = do
  wsn <- newWaylandSurfaceNode ws prt tf
  dcss <- getProgram ShaderDepthCompositedSurface
  dcsbs <-  getProgram ShaderDepthCompositedSurfaceBlitter
  clipping <- getProgram ShaderMotorcarLine

  surfaceTexCoords <- genObjectName
  bindBuffer ArrayBuffer $= Just surfaceTexCoords
  --TODO make this into an utility function
  withArrayLen surfaceVerts $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)

  currentProgram $= Just dcsbs
  colorSamplerBlit <- get $ uniformLocation dcsbs "uColorSampler"
  depthSamplerBlit <- get $ uniformLocation dcsbs "uDepthSampler"
  uniform colorSamplerBlit $= TextureUnit 0
  uniform depthSamplerBlit $= TextureUnit 1
  currentProgram $= Nothing

  ccv <- genObjectName
  bindBuffer ArrayBuffer $= Just ccv
  withArrayLen cuboidClippingVerts $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Word32)), coordPtr, StaticDraw)


  cci <- genObjectName
  bindBuffer ArrayBuffer $= Just cci
  withArrayLen cuboidClippingInds $ \len coordPtr ->
    bufferData ElementArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)

  {-
    wl_array_init(&m_dimensionsArray);
    wl_array_init(&m_transformArray);

    wl_array_add(&m_dimensionsArray, sizeof(glm::vec3));
    wl_array_add(&m_transformArray, sizeof(glm::mat4));
  -}
  
  setNodeTransform (wsn ^. waylandSurfaceNodeDecorations) $ scale identity dims

  MotorcarSurfaceNode
    <$> pure wsn

    <*> pure dcss
    <*> pure dcsbs

    <*> pure clipping
    <*> genObjectName
    <*> genObjectName
    <*> pure surfaceTexCoords
    <*> pure ccv
    <*> pure cci

    <*> get (attribLocation dcss "aPosition")
    <*> get (attribLocation dcss "aColorTexCoord")
    <*> get (attribLocation dcss "aDepthTexCoord")

    <*> get (attribLocation dcsbs "aPosition")
    <*> get (attribLocation dcsbs "aTexCoord")
    <*> pure colorSamplerBlit
    <*> pure depthSamplerBlit

    <*> get (attribLocation clipping "aPosition")
    <*> get (uniformLocation clipping "uMVPMatrix")
    <*> get (uniformLocation clipping "uColor")

    <*> pure dims
    
    
  where
    surfaceVerts = [-1, -1, 0, 1, -1, 0, 1, 1, 0, -1, 1, 0] :: [Float]
    cuboidClippingVerts = [ 0.5, 0.5, 0.5, 0.5, 0.5,-0.5
                          , 0.5,-0.5, 0.5, 0.5,-0.5,-0.5
                          ,-0.5, 0.5, 0.5,-0.5, 0.5,-0.5
                          ,-0.5,-0.5, 0.5,-0.5,-0.5,-0.5
                          ] :: [Float]
    cuboidClippingInds = [0,2,1,1,2,3,4,5,6,5,7,6,0,1,4,1,5,4,2,6,3,3,6,7,0,4,2,2,4,6,1,3,5,3,7,5] :: [Word32]
    


  
