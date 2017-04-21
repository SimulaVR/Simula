module Simula.NewCompositor.SceneGraph where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.IORef
import Data.Typeable
import Data.Word
import Linear

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import Foreign
import Foreign.C

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.Geometry
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Types

Cpp.context Cpp.cppCtx

Cpp.verbatim "#define GL_GLEXT_PROTOTYPES"
Cpp.include "<GL/gl.h>"
Cpp.include "<GL/glext.h>"
Cpp.include "<glm/glm.hpp>"
Cpp.include "<iostream>"

class (Eq a, Typeable a) => SceneGraphNode a where
  nodeOnFrameBegin :: a -> Maybe Scene -> IO ()
  nodeOnFrameBegin _ _ = return ()
  
  nodeOnFrameDraw :: a -> Maybe Scene -> IO ()
  nodeOnFrameDraw _ _ = return ()
  
  nodeOnFrameEnd :: a -> Maybe Scene -> IO ()
  nodeOnFrameEnd _ _ = return ()
  
  nodeOnWorldTransformChange :: a -> Maybe Scene -> IO ()
  nodeOnWorldTransformChange _ _ = return ()

  nodeParent :: a -> IO (Maybe (Some SceneGraphNode))
  setNodeParent' :: a -> Maybe (Some SceneGraphNode) -> IO ()
  nodeChildren :: a -> IORef [Some SceneGraphNode]
  nodeScene :: a -> IO (Maybe Scene)
  nodeScene this = nodeParent this >>= \case
    Just (Some prt) -> nodeScene prt
    Nothing -> return Nothing

  nodeTransform :: a -> IO (M44 Float)
  setNodeTransform' :: a -> M44 Float -> IO ()
  
  isSurfaceNode :: a -> Bool
  isSurfaceNode _ = False

  {- virtual Geometry::RaySurfaceIntersection *intersectWithSurfaces(const Geometry::Ray &ray); -}
  {- Geometry::RaySurfaceIntersection *SceneGraphNode::intersectWithSurfaces(const Geometry::Ray &ray)
{
    Geometry::RaySurfaceIntersection *closestIntersection = NULL, *currentIntersection;
    Geometry::Ray transformedRay = ray.transform(inverseTransform());
    for (SceneGraphNode *child : m_childNodes) {
        if (child != NULL){
            currentIntersection = child->intersectWithSurfaces(transformedRay);
            if(closestIntersection == NULL || (currentIntersection != NULL && currentIntersection->t < closestIntersection->t)){
                delete closestIntersection;
                closestIntersection = currentIntersection;
            }
        }
    }
    return closestIntersection;
}
 -}

setNodeParent :: SceneGraphNode a => a -> Maybe (Some SceneGraphNode) -> IO ()
setNodeParent this Nothing = setNodeParent' this Nothing
setNodeParent this x@(Just (Some prt)) = case cast prt of
  Just prt' | this == prt' -> setNodeParent' this Nothing
  _ -> setNodeParent' this x >> modifyIORef' (nodeChildren prt) (++ [Some this])

nodeSubtreeContains :: (SceneGraphNode a, SceneGraphNode b) => a -> b -> IO Bool
nodeSubtreeContains this node = case cast node of
  Just node' -> return $ this == node'
  Nothing -> readIORef (nodeChildren this) >>= anyM (\(Some child) -> nodeSubtreeContains child node)


setNodeTransform :: SceneGraphNode a => a -> M44 Float -> IO ()
setNodeTransform this tf = do
  setNodeTransform' this tf
  nodeScene this >>= nodeMapOntoSubtree this (\(Some node) -> nodeOnWorldTransformChange node)

nodeWorldTransform :: SceneGraphNode a => a -> IO (M44 Float)
nodeWorldTransform this = nodeParent this >>= \case
  Just (Some prt) -> liftM2 (!*!) (nodeWorldTransform prt) (nodeTransform this)
  Nothing -> nodeTransform this
  
setNodeWorldTransform :: SceneGraphNode a => a -> M44 Float -> IO ()
setNodeWorldTransform this tf = nodeParent this >>= \case
  Just (Some prt) -> fmap (!*! tf) (inv44 <$> nodeWorldTransform prt) >>= setNodeTransform this
  Nothing -> setNodeTransform this tf


nodeMapOntoSubtree :: SceneGraphNode a => a -> (Some SceneGraphNode -> Maybe Scene -> IO ()) -> Maybe Scene -> IO ()
nodeMapOntoSubtree this func scene = do
  func (Some this) scene
  readIORef (nodeChildren this) >>= mapM_ (\(Some child) -> nodeMapOntoSubtree child func scene)

data BaseSceneGraphNode = BaseSceneGraphNode {
  _graphNodeChildren :: IORef [Some SceneGraphNode],
  _graphNodeParent :: IORef (Maybe (Some SceneGraphNode)),
  _graphNodeTransform :: IORef (M44 Float)
  }
  deriving (Eq, Typeable)

baseNodeParent :: BaseSceneGraphNode -> IO (Maybe (Some SceneGraphNode))
baseNodeParent = readIORef . _graphNodeParent

setBaseNodeParent :: BaseSceneGraphNode -> Maybe (Some SceneGraphNode) -> IO ()
setBaseNodeParent = writeIORef . _graphNodeParent

baseNodeTransform :: BaseSceneGraphNode -> IO (M44 Float)
baseNodeTransform = readIORef . _graphNodeTransform

setBaseNodeTransform :: BaseSceneGraphNode -> M44 Float -> IO ()
setBaseNodeTransform = writeIORef . _graphNodeTransform

newBaseNode :: Maybe (Some SceneGraphNode) -> M44 Float -> IO BaseSceneGraphNode
newBaseNode parent tf = BaseSceneGraphNode <$> newIORef [] <*> newIORef parent <*> newIORef tf

class SceneGraphNode a => PhysicalNode a

class SceneGraphNode a => VirtualNode a where
  nodeAnimate :: a -> Int -> IO ()
  nodeAnimate _ _ = return ()

virtualNodeOnFrameBegin :: VirtualNode a => a -> Maybe Scene -> IO ()
virtualNodeOnFrameBegin this (Just scene) = sceneLatestTimestampChange scene >>= nodeAnimate this
virtualNodeOnFrameBegin _ _ = fail "Scene is Nothing"

data Scene = Scene {
  _sceneBase :: BaseSceneGraphNode,
  _sceneCurrentTimestamp :: IORef Int,
  _sceneLastTimestamp :: IORef Int,
  _sceneWindowManager :: IORef (Some WindowManager),
  _sceneCompositor :: IORef (Some Compositor),
  _sceneTrash :: IORef Scene,
  _sceneDisplays :: IORef [Display],
  _sceneActiveDisplay :: IORef (Maybe Display)
  }
  deriving (Eq, Typeable)

instance SceneGraphNode Scene where
  nodeParent = baseNodeParent . _sceneBase
  setNodeParent' = setBaseNodeParent . _sceneBase
  nodeTransform = baseNodeTransform . _sceneBase
  setNodeTransform' = setBaseNodeTransform . _sceneBase
  nodeChildren = _graphNodeChildren . _sceneBase

setSceneTimestamp :: Scene -> Int -> IO ()
setSceneTimestamp this ts = do
  prev <- readIORef $ _sceneCurrentTimestamp this
  writeIORef (_sceneLastTimestamp this) prev
  writeIORef (_sceneCurrentTimestamp this) ts

scenePrepareForFrame :: Scene -> Int -> IO ()
scenePrepareForFrame this ts = do
  setSceneTimestamp this ts
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameBegin node) (Just this)
  dps <- readIORef (_sceneDisplays this)
  forM_ dps $ \dp -> do
    vps <- readIORef (_displayViewpoints dp)
    mapM_ viewPointUpdateViewMatrix vps
    
sceneDrawFrame :: Scene -> IO ()
sceneDrawFrame this = do
  dps <- readIORef (_sceneDisplays this)
  forM_ dps $ \dp -> do
    writeIORef (_sceneActiveDisplay this) (Just dp)
    displayPrepareForDraw dp
    nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameDraw node) (Just this)
    displayFinishDraw dp

sceneFinishFrame :: Scene -> IO ()
sceneFinishFrame this = do
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameEnd node) (Just this)
  {- int error = glGetError();
    if(error != GL_NO_ERROR){
        std::cout <<  "OpenGL Error from frame: " << error <<std::endl;
    } -}

sceneLatestTimestampChange :: Scene -> IO Int
sceneLatestTimestampChange this = do
  last <- readIORef $ _sceneLastTimestamp this
  curr <- readIORef $ _sceneCurrentTimestamp this
  return $ curr - last

data C'wl_global
data C'motorcar_viewpoint


data ViewPoint = ViewPoint {
  _viewPointBase :: BaseSceneGraphNode,
  _viewPointDisplay :: IORef Display,
  _viewPointNear :: IORef Float,
  _viewPointFar :: IORef Float,
  _viewPointViewPort :: IORef ViewPort,
  _viewPointClientColorViewPort :: IORef ViewPort,
  _viewPointClientDepthViewPort :: IORef ViewPort,
  _viewPointCenterOfFocus :: IORef (V4 Float),
  _viewPointCOFTransform :: IORef (M44 Float),
  _viewPointBufferGeometry :: IORef Rectangle,
  _viewPointViewMatrix :: IORef (M44 Float),
  _viewPointProjectionMatrix :: IORef (M44 Float),
  _viewPointViewProjectionMatrix :: IORef (M44 Float),
  _viewPointProjectionMatrixOverriden :: IORef Bool,
  _viewPointViewpointHandle :: IORef (Ptr C'motorcar_viewpoint),
  _viewPointGlobal :: IORef (Ptr C'wl_global),
  _viewPointResources :: IORef [Ptr C'wl_resource]
  } deriving (Eq, Typeable)

instance SceneGraphNode ViewPoint where
  nodeParent = baseNodeParent . _viewPointBase
  setNodeParent' = setBaseNodeParent . _viewPointBase
  nodeTransform = baseNodeTransform . _viewPointBase
  setNodeTransform' = setBaseNodeTransform . _viewPointBase
  nodeChildren = _graphNodeChildren . _viewPointBase

instance VirtualNode ViewPoint

viewPointUpdateViewMatrix :: ViewPoint -> IO ()
viewPointUpdateViewMatrix this = do
  trans <- nodeWorldTransform this
  let center = (trans !* V4 0 0 0 1) ^. _xyz
  let target = (trans !* V4 0 0 (negate 1) 1) ^. _xyz
  let up = normalize $ (trans !* V4 0 1 0 0) ^. _xyz
  writeIORef (_viewPointViewMatrix this) $ lookAt center target up
  sendViewMatrixToClients

  where
    sendViewMatrixToClients = undefined {- std::memcpy(m_viewArray.data, glm::value_ptr(this->viewMatrix()), m_viewArray.size);

    for(struct wl_resource *resource : m_resources){
        motorcar_viewpoint_send_view_matrix(resource, &m_viewArray);
    } -}

viewPointWorldRayAtDisplayPosition :: ViewPoint -> V2 Float -> IO Ray
viewPointWorldRayAtDisplayPosition this pixel = do
  port <- readIORef $ _viewPointViewPort this
  vpCoords <- viewPortDisplayCoordsToViewportCoords port pixel
  let npos = liftI2 (*) (V2 (negate 1) 1) vpCoords

  height <- viewPortHeight port
  width <- viewPortWidth port
  let h = height/width/2

  display <- readIORef $ _viewPointDisplay this
  fov <- viewPointFov this display
  let theta = fov/2
  let d = h / tan theta

  tf <- nodeWorldTransform this
  let ray = Ray (V3 0 0 0) (normalize $ V3 (npos ^. _x) (npos ^. _y) d)
  return $ transformRay ray tf


viewPointFov :: ViewPoint -> Display -> IO Float
viewPointFov this dp = do
  vpTrans <- nodeWorldTransform this
  dpTrans <- nodeWorldTransform dp
  let origin = V4 0 0 0 1
  let ctdVector = (dpTrans !* origin - vpTrans !* origin) ^. _xyz
  let displayNormal = normalize $ (dpTrans !* V4 0 0 1 0) ^. _xyz
  let eyeToScreenDis = abs $ dot ctdVector displayNormal
  dims <- readIORef $ _displayDimensions dp
  return $ 2 * atan ((dims ^. _y)/(2*eyeToScreenDis))

data Display = Display {
  _displayBase :: BaseSceneGraphNode,
  _displayGlContext :: IORef (Some OpenGLContext),
  _displayDimensions :: IORef (V2 Float),
  _displayScratchFrameBuffer :: IORef Word32,
  _displayScratchColorBufferTexture :: IORef Word32,
  _displayScratchDepthBufferTexture :: IORef Word32,
  _displayViewpoints :: IORef [ViewPoint]
  } deriving (Eq, Typeable)

instance SceneGraphNode Display where
  nodeParent = baseNodeParent . _displayBase
  setNodeParent' = setBaseNodeParent . _displayBase
  nodeTransform = baseNodeTransform . _displayBase
  setNodeTransform' = setBaseNodeTransform . _displayBase
  nodeChildren = _graphNodeChildren . _displayBase

newDisplay :: (PhysicalNode a, OpenGLContext b) => b -> V2 Float -> a -> M44 Float -> IO Display
newDisplay glctx dims parent tf = do
  dp <- mkDisplay
  glCtxMakeCurrent glctx
  size <- displaySize dp
  createOrUpdateFBO "Display Scratch Frame Buffer" (_displayScratchFrameBuffer dp) (_displayScratchColorBufferTexture dp) True (_displayScratchDepthBufferTexture dp) True size
  return dp
  where
    mkDisplay = Display
                <$> newBaseNode (Just (Some parent)) tf
                <*> newIORef (Some glctx)
                <*> newIORef dims
                <*> newIORef 0
                <*> newIORef 0
                <*> newIORef 0
                <*> newIORef []

displayWorldRayAtDisplayPosition :: Display -> V2 Float -> IO Ray
displayWorldRayAtDisplayPosition this pixel = do
  cam <- head <$> readIORef (_displayViewpoints this)
  viewPointWorldRayAtDisplayPosition cam pixel

displayWorldPositionAtDisplayPosition :: Display -> V2 Float -> IO (V3 Float)
displayWorldPositionAtDisplayPosition this pixel = do
  worldTf <- nodeWorldTransform this
  size <- (fmap . fmap) fromIntegral $ displaySize this
  dims <- readIORef $ _displayDimensions this
  let scaled = liftI2 (*) (liftI2 (/) pixel (size - V2 0.5 0.5)) dims
  let s4 = worldTf !* V4 (scaled ^. _x) (scaled ^. _y) 0 1
  return $ s4 ^. _xyz


createOrUpdateFBO :: String -> IORef Word32 -> IORef Word32 -> Bool -> IORef Word32 -> Bool -> V2 Int -> IO ()
createOrUpdateFBO fboName fboRef fboColorBufferRef useColorTexture fboDepthBufferRef useDepthTexture resolution = do
  let useColorTexture' = fromBool useColorTexture
  let useDepthTexture' = fromBool useDepthTexture
  let resX = fromIntegral $ resolution ^. _x
  let resY = fromIntegral $ resolution ^. _y
  let resXPow2 = nextPow2 resX
  let resYPow2 = nextPow2 resY
  withCString fboName $ \fboName -> withRef fboRef $ \fboPtr -> withRef fboColorBufferRef $ \fboColorBufferPtr -> withRef fboDepthBufferRef $ \fboDepthBufferPtr ->
    [Cpp.block| void {
    const char* fboName = $(char* fboName);
    GLuint* fbo = $(uint32_t* fboPtr);
    GLuint* fboColorBuffer = $(uint32_t* fboColorBufferPtr);
    GLuint* fboDepthBuffer = $(uint32_t* fboDepthBufferPtr);
    glm::ivec2 resolution = glm::ivec2($(int resX), $(int resY));
    bool useColorTexture = (bool) $(int useColorTexture');
    bool useDepthTexture = (bool) $(int useDepthTexture');
    bool create = !*fbo; //if fbo is null create all objects
    if(create){
        glGenFramebuffers(1, fbo);
        
        if(useColorTexture){
            glGenTextures(1, fboColorBuffer);
            glBindTexture(GL_TEXTURE_2D, *fboColorBuffer);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        }else{
            glGenRenderbuffers(1, fboColorBuffer);
        }
        
        if(useDepthTexture){
            glGenTextures(1, fboDepthBuffer);
            glBindTexture(GL_TEXTURE_2D, *fboDepthBuffer);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        }else{
            glGenRenderbuffers(1, fboDepthBuffer);
        }
        
    }

    glBindFramebuffer(GL_FRAMEBUFFER, *fbo);

    /* calculate the next power of two in both dimensions and use that as a texture size */
    glm::ivec2 fboTexRes = glm::ivec2($(int resXPow2), $(int resYPow2));
    fboTexRes=resolution;

    if(useColorTexture){
        glBindTexture(GL_TEXTURE_2D, *fboColorBuffer);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, fboTexRes.x, fboTexRes.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, *fboColorBuffer, 0);
    }else{
        glBindRenderbuffer(GL_RENDERBUFFER, *fboColorBuffer);
        glRenderbufferStorage(GL_RENDERBUFFER, GL_RGBA8, fboTexRes.x, fboTexRes.y);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, *fboColorBuffer);
    }
    if(useDepthTexture){
        glBindTexture(GL_TEXTURE_2D, *fboDepthBuffer);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, fboTexRes.x, fboTexRes.y, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, 0);
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, *fboDepthBuffer, 0);
    }else{
        glBindRenderbuffer(GL_RENDERBUFFER, *fboDepthBuffer);
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, fboTexRes.x, fboTexRes.y);
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, *fboDepthBuffer);
    }
   
    std::cout << "Checking Framebuffer \"" << std::string(fboName) << "\": ";
    switch(glCheckFramebufferStatus(GL_FRAMEBUFFER)){
            case(GL_FRAMEBUFFER_COMPLETE):
                std::cout << "Framebuffer Complete" << std::endl;
                break;
            case(GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT):
                std::cout << "Framebuffer Attachment Incomplete" << std::endl;
                break;
            case(GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT):
                std::cout << "Framebuffer Attachment Incomplete/Missing" << std::endl;
                break;
            case(GL_FRAMEBUFFER_UNSUPPORTED):
                std::cout << "Framebuffer Unsupported" << std::endl;
                break;
            default:
                std::cout << "Framebuffer is Incomplete for Unknown Reasons" << std::endl;
                break;
    }
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    printf("Successfully %s framebuffer \"%s\": %dx%d (texture size: %dx%d)\n", create ? "created" : "resized",
             fboName, resolution.x, resolution.y, fboTexRes.x, fboTexRes.y);
    } |]

  where
    nextPow2 n
      | countTrailingZeros n + countLeadingZeros n < finiteBitSize n - 1
      =  2^(finiteBitSize n - countLeadingZeros n)
      | otherwise = n

    withRef ref act = do
      val <- readIORef ref
      with val $ \ptr -> act ptr >> peek ptr >>= writeIORef ref
  
  
displaySize :: Display -> IO (V2 Int)
displaySize this = do
  Some glctx <- readIORef (_displayGlContext this)
  glCtxDefaultFramebufferSize glctx
  
displayPrepareForDraw :: Display -> IO ()
displayPrepareForDraw this = do
  Some glctx <- readIORef $ _displayGlContext this
  glCtxMakeCurrent glctx
  [Cpp.block| void {
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
    glClearStencil(0.0);
    glStencilMask(0xFF);
    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc (GL_ONE,GL_ONE_MINUS_SRC_ALPHA);
  } |]

displayFinishDraw :: Display -> IO ()
displayFinishDraw _ = return ()
