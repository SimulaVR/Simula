module Simula.NewCompositor.SceneGraph where

import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Concurrent.MVar
import Data.Typeable
import Linear
import Linear.OpenGL

import Graphics.Rendering.OpenGL hiding (normalize, translate, perspective, lookAt)
import Foreign hiding (void)

import System.Clock

import Simula.WaylandServer
import Simula.MotorcarServer

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.Geometry
import Simula.NewCompositor.OpenGL
import {-# SOURCE #-} Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Types
import Simula.NewCompositor.Utils

data BaseSceneGraphNode = BaseSceneGraphNode {
  _graphNodeChildren :: MVar [Some SceneGraphNode],
  _graphNodeParent :: MVar (Maybe (Some SceneGraphNode)),
  _graphNodeTransform :: MVar (M44 Float)
  } deriving (Eq, Typeable)

--manually generated classy lenses due to TH constraint. probably going to be gone during refactor
class HasBaseSceneGraphNode a where
  baseSceneGraphNode :: Lens' a BaseSceneGraphNode
  graphNodeChildren :: Lens' a (MVar [Some SceneGraphNode])
  graphNodeChildren = baseSceneGraphNode . go
    where go f (BaseSceneGraphNode c p t) = (\c' -> BaseSceneGraphNode c' p t) <$> f c
    
  graphNodeParent :: Lens' a (MVar (Maybe (Some SceneGraphNode)))
  graphNodeParent = baseSceneGraphNode . go
    where go f (BaseSceneGraphNode c p t) = (\p' -> BaseSceneGraphNode c p' t) <$> f p
    
  graphNodeTransform :: Lens' a (MVar (M44 Float))
  graphNodeTransform = baseSceneGraphNode . go
    where go f (BaseSceneGraphNode c p t) = (\t' -> BaseSceneGraphNode c p t') <$> f t

instance HasBaseSceneGraphNode BaseSceneGraphNode where
  baseSceneGraphNode = id

data Scene = Scene {
  _sceneBase :: BaseSceneGraphNode,
  _sceneCurrentTimestamp :: MVar TimeSpec,
  _sceneLastTimestamp :: MVar TimeSpec,
  _sceneWindowManager :: WindowManager,
  _sceneCompositor :: MVar (Some Compositor),
  _sceneDisplays :: MVar [Display],
  _sceneActiveDisplay :: MVar (Maybe Display)
  } deriving (Eq, Typeable)

data ViewPoint = ViewPoint {
  _viewPointBase :: BaseSceneGraphNode,
  _viewPointDisplay :: Display,
  _viewPointNear :: Float,
  _viewPointFar :: Float,
  _viewPointCenterOfFocus :: V4 Float,
  _viewPointCOFTransform :: M44 Float,
  _viewPointBufferGeometry :: Rectangle,
  _viewPointGlobal ::  WlGlobal,
  _viewPointViewArray :: WlArray,
  _viewPointProjectionArray :: WlArray,
  _viewPointResources :: MVar [WlResource],
  _viewPointViewPort :: MVar ViewPort,
  _viewPointClientColorViewPort :: MVar ViewPort,
  _viewPointClientDepthViewPort :: MVar ViewPort,
  _viewPointViewMatrix :: MVar (M44 Float),
  _viewPointProjectionMatrix :: MVar (M44 Float),
  _viewPointProjectionMatrixOverriden :: MVar Bool,
  _viewPointPtr :: StablePtr ViewPoint,
  _viewPointBindFunc :: FunPtr GlobalBindFunc
  } deriving (Eq, Typeable)

data Display = Display {
  _displayBase :: BaseSceneGraphNode,
  _displayGlContext :: Some OpenGLContext,
  _displaySize :: V2 Int,
  _displayDimensions :: V2 Float,
  _displayScratchFrameBuffer :: FramebufferObject,
  _displayScratchColorBufferTexture :: TextureObject,
  _displayScratchDepthBufferTexture :: TextureObject,
  _displayViewpoints :: MVar [ViewPoint]
  } deriving (Eq, Typeable)

data BaseDrawable = BaseDrawable {
  _baseDrawableBase :: BaseSceneGraphNode,
  _baseDrawableVisible :: MVar Bool
  } deriving (Eq, Typeable)

data WireframeNode = WireframeNode {
  _wireframeNodeBase :: BaseDrawable,
  _wireframeNodeLineColor :: MVar (Color3 Float),
  _wireframeNodeSegments :: ForeignPtr Float,
  _wireframeNodeNumSegments :: Int,
  _wireframeNodeLineShader :: Program,
  _wireframeNodeLineVertexCoordinates :: BufferObject,
  _wireframeNodeAPositionLine :: AttribLocation,
  _wireframeNodeUMVPMatrixLine, _wireframeNodeUColorLine :: UniformLocation
  } deriving (Eq, Typeable)

class (Eq a, Typeable a) => SceneGraphNode a where
  nodeDestroy :: a -> IO ()
  nodeDestroy this = do
    prt <- nodeParent this
    case prt of
      Nothing -> return ()
      Just (Some prt) -> modifyMVar' (nodeChildren prt) (filter (/= Some this))
  
  nodeOnFrameBegin :: a -> Maybe Scene -> IO ()
  nodeOnFrameBegin _ _ = return ()
  
  nodeOnFrameDraw :: a -> Maybe Scene -> IO ()
  nodeOnFrameDraw _ _ = return ()
  
  nodeOnFrameEnd :: a -> Maybe Scene -> IO ()
  nodeOnFrameEnd _ _ = return ()
  
  nodeOnWorldTransformChange :: a -> Maybe Scene -> IO ()
  nodeOnWorldTransformChange _ _ = return ()

  nodeParent :: a -> IO (Maybe (Some SceneGraphNode))
  default nodeParent :: HasBaseSceneGraphNode a  => a -> IO (Maybe (Some SceneGraphNode))
  nodeParent = views graphNodeParent readMVar
  
  setNodeParent' :: a -> Maybe (Some SceneGraphNode) -> IO ()
  default setNodeParent' :: HasBaseSceneGraphNode a => a -> Maybe (Some SceneGraphNode) -> IO ()
  setNodeParent' = views graphNodeParent writeMVar
  
  nodeChildren :: a -> MVar [Some SceneGraphNode]
  default nodeChildren :: HasBaseSceneGraphNode a  => a -> MVar [Some SceneGraphNode]
  nodeChildren = view graphNodeChildren
  
  nodeTransform :: a -> IO (M44 Float)
  default nodeTransform :: HasBaseSceneGraphNode a => a -> IO (M44 Float)
  nodeTransform = views graphNodeTransform readMVar
  
  setNodeTransform' :: a -> M44 Float -> IO ()
  default setNodeTransform' :: HasBaseSceneGraphNode a => a -> M44 Float -> IO ()
  setNodeTransform' = views graphNodeTransform writeMVar
  
  nodeScene :: a -> IO (Maybe Scene)
  nodeScene this = nodeParent this >>= \case
    Just (Some prt) -> nodeScene prt
    Nothing -> return Nothing
  
  isSurfaceNode :: a -> Bool
  isSurfaceNode _ = False

  nodeIntersectWithSurfaces :: a -> Ray -> IO (Maybe RaySurfaceIntersection)
  default nodeIntersectWithSurfaces :: HasBaseSceneGraphNode a => a -> Ray -> IO (Maybe RaySurfaceIntersection)
  nodeIntersectWithSurfaces = defaultNodeIntersectWithSurfaces
      

defaultNodeIntersectWithSurfaces :: (SceneGraphNode a, HasBaseSceneGraphNode a) => a -> Ray -> IO (Maybe RaySurfaceIntersection)
defaultNodeIntersectWithSurfaces this ray = do
  tf <- nodeTransform this
  let tfRay = transformRay ray (inv44 tf)
  cs <- readMVar $ nodeChildren this
  foldM (findMinIntersection tfRay) Nothing cs

  where
    findMinIntersection :: Ray -> Maybe RaySurfaceIntersection -> Some SceneGraphNode -> IO (Maybe RaySurfaceIntersection)
    findMinIntersection tfRay Nothing (Some node) = nodeIntersectWithSurfaces node tfRay
    findMinIntersection tfRay prev@(Just closest) (Some node) = do
      current <- nodeIntersectWithSurfaces node tfRay
      case current of
        Just current | current ^. rsiT < closest ^. rsiT -> return $ Just current
        _ -> return prev

class SceneGraphNode a => PhysicalNode a

class SceneGraphNode a => VirtualNode a where
  nodeAnimate :: a -> TimeSpec -> IO ()
  nodeAnimate _ _ = return ()

makeClassy ''BaseDrawable

class VirtualNode a => Drawable a where
  drawableDraw :: a -> Scene -> Display -> IO ()
  drawableVisible :: a -> IO Bool
  default drawableVisible :: HasBaseDrawable a => a -> IO Bool
  drawableVisible = views baseDrawableVisible readMVar

  setDrawableVisible :: a -> Bool -> IO ()
  default setDrawableVisible :: HasBaseDrawable a => a -> Bool -> IO ()
  setDrawableVisible = views baseDrawableVisible writeMVar

-- collate all data structures at the top for proper TH splice scope
makeClassy ''Scene
makeClassy ''ViewPoint
makeClassy ''Display
makeClassy ''WireframeNode

instance Eq (Some SceneGraphNode) where
  Some a == Some b = case cast a of
    Just a -> a == b
    _ -> False

-- classy lens instance
instance HasBaseSceneGraphNode BaseDrawable where
  baseSceneGraphNode = baseDrawableBase

instance HasBaseSceneGraphNode Scene where
  baseSceneGraphNode = sceneBase

instance HasBaseSceneGraphNode ViewPoint where
  baseSceneGraphNode = viewPointBase

instance HasBaseSceneGraphNode Display where
  baseSceneGraphNode = displayBase

instance HasBaseSceneGraphNode WireframeNode where
  baseSceneGraphNode = baseDrawable.baseSceneGraphNode

instance HasBaseDrawable WireframeNode where
  baseDrawable = wireframeNodeBase


setNodeParent :: SceneGraphNode a => a -> Maybe (Some SceneGraphNode) -> IO ()
setNodeParent this Nothing = setNodeParent' this Nothing
setNodeParent this x@(Just (Some prt)) = case cast prt of
  Just prt' | this == prt' -> setNodeParent' this Nothing 
  _ -> do
    setNodeParent' this x
    modifyMVar' (nodeChildren prt) (++ [Some this])

nodeSubtreeContains :: (SceneGraphNode a, SceneGraphNode b) => a -> b -> IO Bool
nodeSubtreeContains this node = case cast node of
  Just node' -> return $ this == node'
  Nothing -> readMVar (nodeChildren this) >>= anyM (\(Some child) -> nodeSubtreeContains child node)


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
  readMVar (nodeChildren this) >>= mapM_ (\(Some child) -> nodeMapOntoSubtree child func scene)

newBaseNode :: SceneGraphNode this => this -> Maybe (Some SceneGraphNode) -> M44 Float -> IO BaseSceneGraphNode
newBaseNode this prt tf = do
  case prt of
    Just (Some prt) -> modifyMVar' (nodeChildren prt) (++ [Some this])
    _ -> return ()
  BaseSceneGraphNode <$> newMVar [] <*> newMVar prt <*> newMVar tf

virtualNodeOnFrameBegin :: VirtualNode a => a -> Maybe Scene -> IO ()
virtualNodeOnFrameBegin this (Just scene) = sceneLatestTimestampChange scene >>= nodeAnimate this
virtualNodeOnFrameBegin _ _ = fail "Scene is Nothing"

instance SceneGraphNode Scene where
  nodeScene this = return $ Just this
instance PhysicalNode Scene

setSceneTimestamp :: Scene -> TimeSpec -> IO ()
setSceneTimestamp this ts = do
  prev <- readMVar $ _sceneCurrentTimestamp this
  writeMVar (_sceneLastTimestamp this) prev
  writeMVar (_sceneCurrentTimestamp this) ts

scenePrepareForFrame :: Scene -> TimeSpec -> IO ()
scenePrepareForFrame this ts = do
  setSceneTimestamp this ts
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameBegin node) (Just this)
  dps <- readMVar (_sceneDisplays this)
  forM_ dps $ \dp -> do
    vps <- readMVar (_displayViewpoints dp)
    mapM_ viewPointUpdateViewMatrix vps
    
sceneDrawFrame :: Scene -> IO ()
sceneDrawFrame this = do
  dps <- readMVar (_sceneDisplays this)
  forM_ dps $ \dp -> do
    writeMVar (_sceneActiveDisplay this) (Just dp)
    displayPrepareForDraw dp
    checkForErrors
    nodeMapOntoSubtree this (\(Some node) scene -> nodeOnFrameDraw node scene >> checkForErrors) (Just this)
    displayFinishDraw dp
    checkForErrors

sceneFinishFrame :: Scene -> IO ()
sceneFinishFrame this = do
  nodeMapOntoSubtree this (\(Some node) -> nodeOnFrameEnd node) (Just this)
  {- int error = glGetError();
    if(error != GL_NO_ERROR){
        std::cout <<  "OpenGL Error from frame: " << error <<std::endl;
    } -}

sceneLatestTimestampChange :: Scene -> IO TimeSpec
sceneLatestTimestampChange this = do
  last <- readMVar $ this ^. sceneLastTimestamp 
  curr <- readMVar $ this ^. sceneCurrentTimestamp
  return $ curr - last

instance SceneGraphNode ViewPoint
instance VirtualNode ViewPoint

newViewPoint :: SceneGraphNode a => Float -> Float -> Display -> a -> M44 Float -> V4 Float -> V3 Float -> IO ViewPoint
newViewPoint near far display parent transform viewPortParams centerOfProjection = do
  let size = display ^. displaySize 
  let bufferGeometry = Rectangle (size & _y *~ 2)
  let vpOffset = viewPortParams ^. _xy
  let vpSize = viewPortParams ^. _zw
  let cofTf = translate centerOfProjection
  let cof = point centerOfProjection

  vport <- ViewPort <$> newMVar vpOffset <*> newMVar vpSize <*> newMVar (Rectangle size)
  let cOff = vpOffset & _y //~ 2
  let cSize = vpSize & _y //~ 2
  ccvp <- ViewPort <$> newMVar cOff <*> newMVar cSize <*> newMVar bufferGeometry
  cdvp <- ViewPort <$> newMVar (cOff & _y +~ (cSize ^. _y)) <*> newMVar cSize <*> newMVar bufferGeometry

  Just scene <- nodeScene parent
  Some comp <- readMVar (scene ^. sceneCompositor)
  let wlDisplay = compositorWlDisplay comp

  vpIf <- motorcarViewpointInterface
  vpVer <- motorcarViewpointVersion

  bindFuncPtr <- createGlobalBindFuncPtr bindFunc
  putStrLn "creating new viewpoint"
  
  rec vp <- ViewPoint base display near far cof cofTf bufferGeometry global
                   <$> newWlArray <*> newWlArray <*> newMVar []
                   <*> newMVar vport <*> newMVar ccvp <*> newMVar cdvp
                   <*> newMVar identity <*> newMVar identity
                   <*> newMVar False <*> pure vpPtr <*> pure bindFuncPtr
      base <- newBaseNode vp (Just (Some parent)) transform
      vpPtr <- newStablePtr vp
      global <- wl_global_create wlDisplay vpIf vpVer (castStablePtrToPtr vpPtr) bindFuncPtr

  putStrLn "created new viewpoint"
  viewPointUpdateViewMatrix vp
  viewPointUpdateProjectionMatrix vp
  viewPointSendViewPortToClients vp
  return vp

  where
    destroyFunc resource = do
      vp :: ViewPoint <- wlResourceData resource >>= deRefStablePtr . castPtrToStablePtr
      modifyMVar' (vp ^. viewPointResources) $ filter (/= resource)
      
    bindFunc client vpPtr version ident = do
      vpIf <- motorcarViewpointInterface
      vp :: ViewPoint <- deRefStablePtr $ castPtrToStablePtr vpPtr
      res <- wl_resource_create client vpIf (fromIntegral version) (fromIntegral ident)
      dFuncPtr <- createResourceDestroyFuncPtr destroyFunc
      wl_resource_set_implementation res nullPtr vpPtr dFuncPtr

      modifyMVar' (vp ^. viewPointResources) (res:)
      viewPointSendCurrentStateToSingleClient vp res


destroyViewPoint :: ViewPoint -> IO ()
destroyViewPoint this = undefined

--TODO these might be wrong
--TODO FIX WORLD TRANSFORM ===========>
viewPointUpdateViewMatrix :: ViewPoint -> IO ()
viewPointUpdateViewMatrix this = do
  trans <- nodeWorldTransform this
  let center = (trans !* V4 0 0 0 1) ^. _xyz
  let target = (trans !* V4 0 0 (negate 1) 1) ^. _xyz
  let up = normalize $ (trans !* V4 0 1 0 0) ^. _xyz
  writeMVar (_viewPointViewMatrix this) $ lookAt center target up
  viewPointSendViewMatrixToClients this

viewPointUpdateProjectionMatrix :: ViewPoint -> IO ()
viewPointUpdateProjectionMatrix this = do
  pmo <- readMVar (this ^. viewPointProjectionMatrixOverriden)
  when (not pmo) $ do
    let cofTf = this ^. viewPointCOFTransform
    let dp = this ^. viewPointDisplay
    fov <- viewPointFov this dp
--    fov <- return $ radians 80
    vport <- readMVar (this ^. viewPointViewPort)
    vpSize <- viewPortSize vport
    let width = vpSize ^. _x
    let height = vpSize ^. _y
    let near = this ^. viewPointNear
    let far = this ^. viewPointFar
    let perM = perspective fov (width/height) near far
    
    writeMVar (this ^. viewPointProjectionMatrix) $ cofTf !*! perM
  viewPointSendProjectionMatrixToClients this


viewPointOverrideProjectionMatrix :: ViewPoint -> M44 Float -> IO ()
viewPointOverrideProjectionMatrix this mat = do
  writeMVar (this ^. viewPointProjectionMatrix) mat
  writeMVar (this ^. viewPointProjectionMatrixOverriden) True
  viewPointUpdateProjectionMatrix this


--TODO refactor
viewPointSendViewMatrixToClients :: ViewPoint -> IO ()
viewPointSendViewMatrixToClients this = do
  viewMatrix <- readMVar (this ^. viewPointViewMatrix)
  let array = this ^. viewPointViewArray
  wlArrayOverwrite array (transpose viewMatrix) -- column-major

  resources <- readMVar (this ^. viewPointResources)
  forM_ resources $ \res -> motorcar_viewpoint_send_view_matrix res array

--TODO refactor
viewPointSendProjectionMatrixToClients :: ViewPoint -> IO ()
viewPointSendProjectionMatrixToClients this = do
  projMatrix <- readMVar (this ^. viewPointProjectionMatrix)
  let array = this ^. viewPointProjectionArray
  wlArrayOverwrite array (transpose projMatrix) -- column-major

  resources <- readMVar (this ^. viewPointResources)
  forM_ resources $ \res -> motorcar_viewpoint_send_projection_matrix res array

viewPointSendViewPortToClients :: ViewPoint -> IO ()
viewPointSendViewPortToClients this
  = readMVar (this ^. viewPointResources) >>= mapM_ (viewPointSendViewPortToSingleClient this)


viewPointSendViewPortToSingleClient :: ViewPoint -> WlResource -> IO ()
viewPointSendViewPortToSingleClient this res = do
  ccvp <- readMVar (this ^. viewPointClientColorViewPort)
  cdvp <- readMVar (this ^. viewPointClientDepthViewPort)
  --TODO deal with Float
  ccvpOff <- (fmap.fmap) truncate $ viewPortOffset ccvp
  cdvpOff <- (fmap.fmap) truncate $ viewPortOffset cdvp
  ccvpSize <- (fmap.fmap) truncate $ viewPortSize ccvp
  cdvpSize <- (fmap.fmap) truncate $ viewPortSize cdvp
  
  motorcar_viewpoint_send_view_port res
    (ccvpOff ^. _x) (ccvpOff ^. _y) (ccvpSize ^. _x) (ccvpSize ^. _y)
    (cdvpOff ^. _x) (cdvpOff ^. _y) (cdvpSize ^. _x) (cdvpSize ^. _y)


viewPointSendCurrentStateToSingleClient :: ViewPoint -> WlResource -> IO ()
viewPointSendCurrentStateToSingleClient this res = do
  motorcar_viewpoint_send_view_matrix res (this ^. viewPointViewArray)
  motorcar_viewpoint_send_projection_matrix res (this ^. viewPointProjectionArray)
  viewPointSendViewPortToSingleClient this res



viewPointWorldRayAtDisplayPosition :: ViewPoint -> V2 Float -> IO Ray
viewPointWorldRayAtDisplayPosition this pixel = do
  port <- readMVar $ _viewPointViewPort this
  vpCoords <- viewPortDisplayCoordsToViewportCoords port pixel
  let npos = liftI2 (*) (V2 (negate 1) 1) vpCoords

  size <- viewPortSize port
  let height = size ^. _y
  let width = size ^. _x
  let h = height/width/2

  let display = this ^. viewPointDisplay
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
  let ctdVector = ((dpTrans !* origin) - (vpTrans !* origin)) ^. _xyz
  let displayNormal = normalize $ (dpTrans !* V4 0 0 1 0) ^. _xyz
  let eyeToScreenDis = abs $ dot ctdVector displayNormal
  let dims = _displayDimensions dp
  return $ 2 * atan ((dims ^. _y)/(2*eyeToScreenDis))

instance SceneGraphNode Display

newDisplay :: (PhysicalNode a, OpenGLContext ctx) => ctx -> V2 Int -> V2 Float -> a -> M44 Float -> IO Display
newDisplay glctx size dims parent tf = do
  glCtxMakeCurrent glctx
  (fbo, fboCb, fboDb) <- createFBO size
  rec dp <- Display base
            <$> pure (Some glctx)
            <*> pure size
            <*> pure dims
            <*> pure fbo
            <*> pure fboCb
            <*> pure fboDb
            <*> newMVar []
      base <- newBaseNode dp (Just (Some parent)) tf
  return dp
  

displayWorldRayAtDisplayPosition :: Display -> V2 Float -> IO Ray
displayWorldRayAtDisplayPosition this pixel = do
  cam <- head <$> readMVar (_displayViewpoints this)
  viewPointWorldRayAtDisplayPosition cam pixel

displayWorldPositionAtDisplayPosition :: Display -> V2 Float -> IO (V3 Float)
displayWorldPositionAtDisplayPosition this pixel = do
  worldTf <- nodeWorldTransform this
  let size = fromIntegral <$> this ^. displaySize 
  let dims = _displayDimensions this
  let scaled = liftI2 (*) (liftI2 (/) pixel (size - V2 0.5 0.5)) dims
  let s4 = worldTf !* V4 (scaled ^. _x) (scaled ^. _y) 0 1
  return $ s4 ^. _xyz


createFBO :: V2 Int -> IO (FramebufferObject, TextureObject, TextureObject)
createFBO resolution = do
  let resX = fromIntegral $ resolution ^. _x
  let resY = fromIntegral $ resolution ^. _y

  fbo <- genObjectName
  fboColorBuffer <- genObjectName
  textureBinding Texture2D $= Just fboColorBuffer
  textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
        
  fboDepthBuffer <- genObjectName
  textureBinding Texture2D $= Just fboDepthBuffer
  textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  bindFramebuffer Framebuffer $= fbo

  let size = TextureSize2D resX resY

  textureBinding Texture2D $= Just fboColorBuffer
  texImage2D Texture2D NoProxy 0 RGBA' size 0 (PixelData RGBA UnsignedByte nullPtr)
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D fboColorBuffer 0

  textureBinding Texture2D $= Just fboDepthBuffer
  texImage2D Texture2D NoProxy 0 Depth24Stencil8 size 0 (PixelData DepthStencil UnsignedInt248 nullPtr)
  framebufferTexture2D Framebuffer DepthStencilAttachment Texture2D fboDepthBuffer 0
  bindFramebuffer Framebuffer $= defaultFramebufferObject
  checkForErrors

  putStrLn "created FBO"
  return (fbo, fboColorBuffer, fboDepthBuffer)

    
displayPrepareForDraw :: Display -> IO ()
displayPrepareForDraw this = do
  Some glctx <- return $ this ^. displayGlContext
  glCtxMakeCurrent glctx

  clearColor $= Color4 1 1 1 1
  clearStencil $= 0
  stencilMask $= 0xff
  clear [ColorBuffer, DepthBuffer, StencilBuffer]
  blend $= Enabled
  blendFunc $= (One, OneMinusSrcAlpha)
  checkForErrors


displayFinishDraw :: Display -> IO ()
displayFinishDraw _ = return ()

drawableOnFrameDraw :: Drawable a => a -> Maybe Scene -> IO ()
drawableOnFrameDraw this (Just scene) = do
  visible <- drawableVisible this
  -- display is always activated beforehand
  Just display <- readMVar $ _sceneActiveDisplay scene
  when visible $ drawableDraw this scene display
  checkForErrors
drawableOnFrameDraw _ _ = return ()

newBaseDrawable :: SceneGraphNode this => this -> Maybe (Some SceneGraphNode) -> M44 Float -> IO BaseDrawable
newBaseDrawable this parent tf = BaseDrawable <$> newBaseNode this parent tf <*> newMVar True

instance SceneGraphNode WireframeNode where
  nodeOnFrameDraw = drawableOnFrameDraw

instance VirtualNode WireframeNode

instance Drawable WireframeNode where
  drawableDraw this _ display = withForeignPtr (_wireframeNodeSegments this) $ \segPtr -> do
    currentProgram $= Just (_wireframeNodeLineShader this)
    vertexAttribArray (_wireframeNodeAPositionLine this) $= Enabled
    bindBuffer ArrayBuffer $= Just (_wireframeNodeLineVertexCoordinates this)
    vertexAttribPointer (_wireframeNodeAPositionLine this) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
    bufferData ArrayBuffer $= (fromIntegral $ _wireframeNodeNumSegments this * 6 * sizeOf (undefined :: Float), segPtr, DynamicDraw)

    lineColor <- readMVar $ _wireframeNodeLineColor this
    
    uniform (_wireframeNodeUColorLine this) $= lineColor

    viewpoints <- readMVar $ _displayViewpoints display
    forM_ viewpoints $ \vp -> do
      projMatrix <- readMVar $ _viewPointProjectionMatrix vp
      viewMatrix <- readMVar $ _viewPointViewMatrix vp
      worldTf <- nodeWorldTransform this
      
      let mat = (projMatrix !*! viewMatrix !*! worldTf) ^. m44GLmatrix
      uniform (_wireframeNodeUMVPMatrixLine this) $= mat

      port <- readMVar $ _viewPointViewPort vp
      setViewPort port

      drawArrays Lines 0 (fromIntegral $ 2 * _wireframeNodeNumSegments this)
      checkForErrors

    vertexAttribArray (_wireframeNodeAPositionLine this) $= Disabled
    currentProgram $= Nothing
    checkForErrors

newWireframeNode :: [Float] -> Color3 Float -> Maybe (Some SceneGraphNode) -> M44 Float -> IO WireframeNode
newWireframeNode segs lineColor parent transform = do
  lineColorRef <- newMVar lineColor
  segArray <- newArray segs >>= newForeignPtr finalizerFree
  let numSegments = length segs `div` 6
  coordsBuffer <- genObjectName
  prog <- getProgram ShaderMotorcarLine
  aPos <- get $ attribLocation prog "aPosition"
  uMVP <- get $ uniformLocation prog "uMVPMatrix"
  uColor <- get $ uniformLocation prog "uColor"
  checkForErrors
  rec let node = WireframeNode drawable lineColorRef segArray numSegments prog coordsBuffer aPos uMVP uColor
      drawable <- newBaseDrawable node parent transform
  return node
