module Simula.NewCompositor.Weston where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.IORef
import Data.Word
import Data.Typeable
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL hiding (scale, translate, rotate, Rect)
import Linear
import Linear.OpenGL
import System.Clock
import System.Environment

import Simula.WaylandServer
import Simula.Weston

import Simula.NewCompositor.Compositor
import Simula.NewCompositor.Geometry
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Wayland.Output
import Simula.NewCompositor.Utils
import Simula.NewCompositor.Types

data SimulaSurface = SimulaSurface {
  _simulaSurfaceBase :: BaseWaylandSurface,
  _simulaSurfaceWestonSurface :: WestonSurface,
  _simulaSurfaceCompositor :: SimulaCompositor,
  _simulaSurfaceTexture :: IORef (Maybe TextureObject)
  } deriving (Eq, Typeable)

data SimulaCompositor = SimulaCompositor {
  _simulaCompositorScene :: Scene,
  _simulaCompositorDisplay :: WlDisplay,
  _simulaCompositorWestonCompositor :: WestonCompositor,
  _simulaCompositorSurfaceCreatedFuncPtr :: FunPtr (NotifyFunc WestonSurface),
  _simulaCompositorSurfaceMap :: IORef (M.Map WestonSurface SimulaSurface),
  _simulaCompositorOpenGlData :: OpenGLData
  } deriving Eq

data SimulaSeat = SimulaSeat

data OpenGLData = OpenGLData {
  _openGlDataPpcm :: Float,
  _openGlDataTextureBlitter :: TextureBlitter,
  _openGLDataSurfaceFbo :: FramebufferObject
  } deriving Eq

data TextureBlitter = TextureBlitter {
  _textureBlitterProgram :: Program,
  _textureBlitterVertexCoordEntry :: AttribLocation,
  _textureBlitterTextureCoordEntry :: AttribLocation,
  _textureBlitterMatrixLocation :: UniformLocation
  } deriving Eq

makeLenses ''SimulaSurface
makeLenses ''SimulaCompositor
makeLenses ''OpenGLData
makeLenses ''TextureBlitter

instance HasBaseWaylandSurface SimulaSurface where
  baseWaylandSurface = simulaSurfaceBase

newSimulaSurface :: WestonSurface -> SimulaCompositor -> WaylandSurfaceType -> IO SimulaSurface
newSimulaSurface ws comp ty = SimulaSurface
                              <$> newBaseWaylandSurface ty
                              <*> pure ws <*> pure comp
                              <*> newIORef Nothing

newOpenGlData :: IO OpenGLData
newOpenGlData = OpenGLData 64 <$> newTextureBlitter <*> genObjectName

newTextureBlitter :: IO TextureBlitter
newTextureBlitter = do
  program <- getProgram ShaderTextureBlitter
  blend $= Enabled
  blendFunc $= (One, OneMinusSrcAlpha)
  TextureBlitter
    <$> pure program
    <*> get (attribLocation program "vertexCoordEntry")
    <*> get (attribLocation program "textureCoordEntry")
    <*> get (uniformLocation program "matrix")

bindTextureBlitter :: TextureBlitter -> IO ()
bindTextureBlitter tb = currentProgram $= Just (tb ^. textureBlitterProgram)

blitterDrawTexture :: TextureBlitter -> TextureObject -> Rect Float -> V2 Int -> Int -> Bool -> Bool -> IO ()
blitterDrawTexture tb tex targetRect targetSize depth targetInvertedY sourceInvertedY = do
  viewport $= (Position 0 0, Size (fromIntegral $ targetSize ^. _x) (fromIntegral $ targetSize ^. _y))

  let vertexCoordEntry = tb ^. textureBlitterVertexCoordEntry
  let textureCoordEntry = tb ^. textureBlitterTextureCoordEntry
  let matrix = tb ^. textureBlitterMatrixLocation

  vertexAttribArray vertexCoordEntry $= Enabled
  vertexAttribArray textureCoordEntry $= Enabled

  withArrayLen vertexCoordinates $ \len arrPtr ->
    vertexAttribPointer vertexCoordEntry $= (ToFloat, VertexArrayDescriptor 3 Float 0 arrPtr)
  withArrayLen textureCoordinates $ \len arrPtr ->
    vertexAttribPointer textureCoordEntry $= (ToFloat, VertexArrayDescriptor 2 Float 0 arrPtr)

  uniform matrix $= transform ^. m44GLmatrix

  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  drawArrays TriangleFan 0 4

  textureBinding Texture2D $= Nothing

  vertexAttribArray vertexCoordEntry $= Disabled
  vertexAttribArray textureCoordEntry $= Disabled

  where
    z = fromIntegral depth / 1000
    
    textureCoordinates = [ 0, 0
                         , 1, 0
                         , 1, 1
                         , 0, 1 ] :: [Float]
    vertexCoordinates =  [ x1, y1, z
                         , x2, y1, z
                         , x2, y2, z
                         , x1, y2, z ] :: [Float]
    
    x1 = rectLeft targetRect
    x2 = rectRight targetRect
    invertTarget y | targetInvertedY = y
                   | otherwise = (fromIntegral $ targetSize ^. _y) - y

    invertSource y1 y2 | sourceInvertedY = (y1, y2)
                       | otherwise = (y2, y1)
  
    (y1', y2') = invertSource (rectTop targetRect) (rectBottom targetRect)
    y1 = invertTarget y1'
    y2 = invertTarget y2'

    width = fromIntegral $ targetSize ^. _x :: Float
    height = fromIntegral $ targetSize ^. _y :: Float
    
    scaleMat = scale $ V3 (2 / width) (2 / height) 1
    translateMat = translate $ V3 (negate width / 2) (negate height / 2) 0
    transform = translateMat !*! scaleMat :: M44 Float
  

setTimeout :: IO () -> Int -> IO ThreadId
setTimeout ioOperation ms =
  forkIO $ do
    threadDelay (ms*1000)
    ioOperation

--BIG TODO: type safety for C bindings, e.g. WlSignal should encode the type of the NotifyFunc data.

instance Compositor SimulaCompositor where
  compositorWlDisplay = view simulaCompositorDisplay
  compositorGetSurfaceFromResource comp resource = do
    ptr <- wlResourceData resource
    let surface = WestonSurface (castPtr ptr)
    Some <$> newSimulaSurface surface comp NA
    

newSimulaCompositor :: Scene -> IO SimulaCompositor
newSimulaCompositor scene = do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr

  setup_weston_log_handler
  westonCompositorSetEmptyRuleNames wcomp

  with (WestonBackendConfig westonWaylandBackendConfigVersion 0) $ weston_compositor_load_backend wcomp WestonBackendX11
  
  socketName <- wl_display_add_socket_auto wldp
  setEnv "WAYLAND_DISPLAY" socketName

  rec surfaceCreatedPtr <- createNotifyFuncPtr (onSurfaceCreated compositor)
      compositor <- SimulaCompositor scene wldp wcomp surfaceCreatedPtr
                    <$> newIORef M.empty <*> newOpenGlData
      
  let surfaceCreatedSignal = westonCompositorCreateSurfaceSignal wcomp

  addListenerToSignal surfaceCreatedSignal surfaceCreatedPtr

  return compositor

 where
   onSurfaceCreated compositor _ sfPtr = do
     let surface = WestonSurface (castPtr sfPtr)
     simulaSurface <- newSimulaSurface surface compositor NA
     modifyIORef' (compositor ^. simulaCompositorSurfaceMap) (M.insert surface simulaSurface)

   onSurfaceDestroyed compositor _ sfPtr = do
     let surface = WestonSurface (castPtr sfPtr)
     --TODO destroy surface in wm
     modifyIORef' (compositor ^. simulaCompositorSurfaceMap) (M.delete surface)

   onSurfaceCommit = undefined




instance WaylandSurface SimulaSurface where
  wsTexture = views simulaSurfaceTexture readIORef
  
  wsSize surf = V2 <$> westonSurfaceWidth ws <*> westonSurfaceHeight ws
    where
      ws = surf ^. simulaSurfaceWestonSurface
  
  setWsSize surf (V2 x y) = setWestonSurfaceWidth ws x >> setWestonSurfaceHeight ws y
    where
      ws = surf ^. simulaSurfaceWestonSurface
  
  wsPosition surf = do
    (view:_) <- westonSurfaceViews ws
    V2 <$> westonViewPosX view <*> westonViewPosY view

    where
      ws = surf ^. simulaSurfaceWestonSurface
    
  wsPrepare surf = do
    texture <- composeSurface surf (surf ^. simulaSurfaceCompositor.simulaCompositorOpenGlData)
    oldTex <- wsTexture surf 
    case oldTex of
      Nothing -> return ()
      Just oldTex -> do
        deleteObjectName oldTex
    writeIORef (surf ^. simulaSurfaceTexture) (Just texture)
  
  wsSendEvent surf event = undefined


textureFromSurface :: WestonSurface -> IO TextureObject
textureFromSurface ws = do
  texture <- genObjectName
  textureBinding Texture2D $= Just texture

  buffer <- wl_shm_buffer_get . westonBufferResource $ westonSurfaceBuffer ws

  format <- wl_shm_buffer_get_format buffer
  width <- fromIntegral <$> wl_shm_buffer_get_width buffer
  height <- fromIntegral <$> wl_shm_buffer_get_height buffer

  --TODO support more formats
  when (format `notElem` [WlShmFormatArgb8888, WlShmFormatXrgb8888]) . ioError . userError $ "Unsupported pixel format " ++ show format
  
  withShmBuffer buffer $ \ptr -> do
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D width height) 0 (PixelData BGRA UnsignedInt8888Rev ptr)

  textureBinding Texture2D $= Nothing
  return texture


composeSurface :: SimulaSurface -> OpenGLData -> IO TextureObject
composeSurface surf gld = do
  let ws = surf ^. simulaSurfaceWestonSurface
  size <- V2 <$> westonSurfaceWidth ws <*> westonSurfaceHeight ws

  let fbo = gld ^. openGLDataSurfaceFbo
  bindFramebuffer Framebuffer $= fbo

  texture <- textureFromSurface ws

  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D texture 0
  paintChildren ws ws size gld

  --TODO what does this do?
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D (TextureObject 0) 0
  bindFramebuffer Framebuffer $= defaultFramebufferObject

  return texture
  

paintChildren :: WestonSurface -> WestonSurface -> V2 Int -> OpenGLData -> IO ()
paintChildren surface window windowSize gld = do
  subsurfaces <- westonSurfaceSubsurfaces surface

  when (not $ null subsurfaces) $ forM_ subsurfaces $ \ss -> do
    let subsurface = westonSubsurfaceSurface ss

    (sView:_) <- westonSurfaceViews surface
    (ssView:_) <- westonSurfaceViews subsurface

    sPos <- westonViewPos sView
    ssPos <- westonViewPos ssView
    let p = sPos ^+^ ssPos

    windowSize <- westonSurfaceSize window
    subSize <- westonSurfaceSize subsurface

    -- .isValid() checks for all (>)
    when (all (>0) subSize) $ do
      tex <- textureFromSurface subsurface
      let geo = Rect p (fromIntegral <$> subSize)
      windowInverted <- westonSurfaceIsYInverted window
      subsurfaceInverted <- westonSurfaceIsYInverted subsurface
      blitterDrawTexture (gld ^. openGlDataTextureBlitter) tex geo windowSize 0 windowInverted subsurfaceInverted
      deleteObjectName tex
    paintChildren subsurface window windowSize gld
    

compositorRender :: SimulaCompositor -> IO ()
compositorRender comp = do
  surfaceMap <- readIORef (comp ^. simulaCompositorSurfaceMap)
  let surfaces = M.keys surfaceMap
  let scene = comp ^. simulaCompositorScene

  time <- getTime Realtime
  
  scenePrepareForFrame scene time
  -- notify surfaces about frame? weston_output.frame_signal?
  -- weston_surface_schedule_repaint?
  
  moveCamera
  sceneDrawFrame scene
  sceneFinishFrame scene

  where
    moveCamera = return ()
{-
    if(m_camIsMoving) {
        glm::vec4 camPos;
        camPos *= 0;
        camPos.w = 1;
        glm::vec4 delta = camPos;
        delta.x = m_camMoveVec.x;
        delta.y = m_camMoveVec.y;
        delta.z = m_camMoveVec.z;

        const float speed = 0.01;
        delta *= speed;
        delta.w /= speed;
        glm::mat4 trans = display()->transform();
        //camPos = trans * camPos;
        //delta = trans * delta;
        glm::vec3 move = glm::vec3(delta.x/delta.w - camPos.x/camPos.w, delta.y/delta.w - camPos.y/camPos.w,
                                   delta.z/delta.w - camPos.z/camPos.w);
        trans = glm::translate(trans, move);
        display()->setTransform(trans);
    }
-}
      
