module Simula.NewCompositor.Weston where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.Hashable
import Data.Word
import Data.Typeable
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL hiding (scale, translate, rotate, Rect)
import Linear
import Linear.OpenGL
import System.Clock
import System.Environment
import System.Mem.StableName
import Simula.WaylandServer
import Simula.Weston
import Simula.WestonDesktop


import Simula.NewCompositor.Compositor
import Simula.NewCompositor.Geometry
import Simula.NewCompositor.OpenGL
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Wayland.Output
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.Utils
import Simula.NewCompositor.Types

data SimulaSurface = SimulaSurface {
  _simulaSurfaceBase :: BaseWaylandSurface,
  _simulaSurfaceWestonDesktopSurface :: WestonDesktopSurface,
  _simulaSurfaceView :: WestonView,
  _simulaSurfaceCompositor :: MVar SimulaCompositor,
  _simulaSurfaceTexture :: MVar (Maybe TextureObject),
  _simulaSurfaceStableName :: StableName SimulaSurface
  } deriving (Eq, Typeable)

instance Hashable SimulaSurface where
  hashWithSalt s = hashWithSalt s . _simulaSurfaceStableName

data SimulaCompositor = SimulaCompositor {
  _simulaCompositorScene :: Scene,
  _simulaCompositorDisplay :: Display,
  _simulaCompositorWlDisplay :: WlDisplay,
  _simulaCompositorWestonCompositor :: WestonCompositor,
  _simulaCompositorSurfaceMap :: MVar (M.Map WestonSurface SimulaSurface),
  _simulaCompositorOpenGlData :: OpenGLData,
  _simulaCompositorOutput :: MVar (Maybe WestonOutput),
  _simulaCompositorGlContext :: MVar (Maybe SimulaOpenGLContext),
  _simulaCompositorNormalLayer :: WestonLayer
  } deriving Eq

data SimulaSeat = SimulaSeat {
  _simulaSeatBase :: BaseSeat
  } deriving (Eq, Typeable)

data OpenGLData = OpenGLData {
  _openGlDataPpcm :: Float,
  _openGlDataTextureBlitter :: TextureBlitter,
  _openGlDataSurfaceFbo :: FramebufferObject
  } deriving Eq

data SimulaOpenGLContext = SimulaOpenGLContext {
  _simulaOpenGlContextEglContext :: EGLContext,
  _simulaOpenGlContextEglDisplay :: EGLDisplay,
  _simulaOpenGlContextEglSurface :: EGLSurface
  } deriving (Eq, Typeable)

data TextureBlitter = TextureBlitter {
  _textureBlitterProgram :: Program,
  _textureBlitterVertexCoordEntry :: AttribLocation,
  _textureBlitterTextureCoordEntry :: AttribLocation,
  _textureBlitterMatrixLocation :: UniformLocation
  } deriving Eq

makeLenses ''SimulaSurface
makeLenses ''SimulaCompositor
makeLenses ''OpenGLData
makeLenses ''SimulaOpenGLContext
makeLenses ''TextureBlitter
makeLenses ''SimulaSeat

instance HasBaseSeat SimulaSeat where
  baseSeat = simulaSeatBase

instance Seat SimulaSeat

instance OpenGLContext SimulaOpenGLContext where
  glCtxMakeCurrent this = do
    eglMakeCurrent egldp eglsurf eglsurf eglctx

    where
      eglctx = this ^. simulaOpenGlContextEglContext
      egldp = this ^. simulaOpenGlContextEglDisplay
      eglsurf = this ^. simulaOpenGlContextEglSurface



instance HasBaseWaylandSurface SimulaSurface where
  baseWaylandSurface = simulaSurfaceBase

newSimulaSeat :: IO SimulaSeat
newSimulaSeat = SimulaSeat <$> newBaseSeat

newSimulaSurface :: WestonDesktopSurface -> WestonView -> SimulaCompositor -> WaylandSurfaceType -> IO SimulaSurface
newSimulaSurface ws view comp ty = do
  rec surface <- SimulaSurface
                 <$> newBaseWaylandSurface ty
                 <*> pure ws <*> pure view
                 <*> newMVar comp
                 <*> newMVar Nothing
                 <*> makeStableName surface
  return surface

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
  checkForErrors

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
  

setTimeout :: Int -> IO () -> IO ThreadId
setTimeout ms ioOperation =
  forkIO $ do
    threadDelay (ms*1000)
    ioOperation

--BIG TODO: type safety for C bindings, e.g. WlSignal should encode the type of the NotifyFunc data.

instance Compositor SimulaCompositor where
  compositorDisplay = return . view simulaCompositorDisplay
  compositorWlDisplay = view simulaCompositorWlDisplay
  compositorOpenGLContext this = do
    Just glctx <- readMVar (this ^. simulaCompositorGlContext)
    return (Some glctx)
    
  compositorGetSurfaceFromResource comp resource = do
    ptr <- wlResourceData resource    
    let ws = WestonSurface (castPtr ptr)
    surface <- weston_surface_get_desktop_surface ws
    putStr "resource ptr: "
    print ptr
    simulaSurface <- M.lookup ws <$> readMVar (comp ^. simulaCompositorSurfaceMap)
    case simulaSurface of
      Just simulaSurface -> return (Some simulaSurface)
      _ -> do
        simulaSurface <- createSurface comp surface
        return (Some simulaSurface)
    

setSurfaceMapped :: SimulaSurface -> Bool -> IO ()
setSurfaceMapped simulaSurface status = do
  ws <- weston_desktop_surface_get_surface surface
  westonSurfaceMapped ws >>= go ws
  where
    surface = simulaSurface ^. simulaSurfaceWestonDesktopSurface
    view = simulaSurface ^. simulaSurfaceView
    go ws currentStatus
      | status && not currentStatus = do
          westonSurfaceSetMapped ws True
          westonViewSetMapped view True
          weston_view_update_transform view
          compositor <- readMVar (simulaSurface ^. simulaSurfaceCompositor)
          let wm = compositor ^. simulaCompositorScene.sceneWindowManager
          wmMapSurface wm simulaSurface TopLevel
          Just output <- readMVar (compositor ^. simulaCompositorOutput)
          weston_output_schedule_repaint output
      | not status && currentStatus = do
          westonSurfaceSetMapped ws False
          westonViewSetMapped view False
          compositor <- readMVar (simulaSurface ^. simulaSurfaceCompositor)
          let wm = compositor ^. simulaCompositorScene.sceneWindowManager
          wmUnmapSurface wm simulaSurface
      | otherwise = return ()

  

createSurface :: SimulaCompositor -> WestonDesktopSurface  -> IO SimulaSurface
createSurface compositor surface = do
  view <- weston_desktop_surface_create_view surface
  ws <- weston_desktop_surface_get_surface surface
  simulaSurface <- M.lookup ws <$> readMVar (compositor ^. simulaCompositorSurfaceMap)
  case simulaSurface of
    Just s -> return s
    _ -> do
      simulaSurface <- newSimulaSurface surface view compositor NA
      modifyMVar' (compositor ^. simulaCompositorSurfaceMap) (M.insert ws simulaSurface)
  
      Just output <- readMVar (compositor ^. simulaCompositorOutput)
      westonViewSetOutput view output
      let layer = compositor ^. simulaCompositorNormalLayer
            
      weston_layer_entry_insert  (westonLayerViewList layer) (westonViewLayerEntry view)
      
      return simulaSurface

--BUG TODO: need an actual wl_shell
newSimulaCompositor :: Scene -> Display -> IO SimulaCompositor
newSimulaCompositor scene display = do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr

  setup_weston_log_handler
  westonCompositorSetEmptyRuleNames wcomp

  --todo hack; make this into a proper withXXX function
  res <- with (WestonX11BackendConfig (WestonBackendConfig westonX11BackendConfigVersion (sizeOf (undefined :: WestonX11BackendConfig)))
           False
           False
           False) $ weston_compositor_load_backend wcomp WestonBackendX11 . castPtr

  when (res > 0) $ ioError $ userError "Error when loading backend"
  
  socketName <- wl_display_add_socket_auto wldp
  putStrLn $ "Socket: " ++ socketName
  setEnv "WAYLAND_DISPLAY" socketName

  mainLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionNormal
  bgLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionBackground


  compositor <- SimulaCompositor scene display wldp wcomp
                <$> newMVar M.empty <*> newOpenGlData
                <*> newMVar Nothing <*> newMVar Nothing
                <*> pure mainLayer

  windowedApi <- weston_windowed_output_get_api wcomp

  let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
  outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi compositor)
  addListenerToSignal outputPendingSignal outputPendingPtr

  let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  outputCreatedPtr <- createNotifyFuncPtr (onOutputCreated compositor)
  addListenerToSignal outputCreatedSignal outputCreatedPtr
 
  westonWindowedOutputCreate windowedApi wcomp "X"


  let api = defaultWestonDesktopApi {
        apiSurfaceAdded = onSurfaceCreated compositor,
        apiSurfaceRemoved = onSurfaceDestroyed compositor,
        apiCommitted = onSurfaceCommit compositor
        }

  
  westonDesktopCreate wcomp api nullPtr

  return compositor

  where
    onSurfaceCreated compositor surface  _ = do
      putStrLn "surface created"
      createSurface compositor surface
      return ()


    onSurfaceDestroyed compositor surface _ = do
      --TODO destroy surface in wm
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. simulaCompositorSurfaceMap) 
      case simulaSurface of
        Just simulaSurface -> do
          modifyMVar' (compositor ^. simulaCompositorSurfaceMap) (M.delete ws)
          let wm = compositor ^. simulaCompositorScene.sceneWindowManager
          setSurfaceMapped simulaSurface False
          wmDestroySurface wm simulaSurface
        _ -> return ()
  
    onSurfaceCommit compositor surface x y _ = do
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. simulaCompositorSurfaceMap)
      case simulaSurface of
        Just simulaSurface -> do
          setSurfaceMapped simulaSurface True
          -- need to figure out surface type
          return ()
        _ -> return ()

    onOutputPending windowedApi compositor _ outputPtr = do
      putStrLn "output pending"
      let output = WestonOutput $ castPtr outputPtr
      --TODO hack
      weston_output_set_scale output 1
      weston_output_set_transform output 0
      westonWindowedOutputSetSize windowedApi output 1280 720

      weston_output_enable output
      return ()


    onOutputCreated compositor _ outputPtr = do
      putStrLn "output created"
      let output = WestonOutput $ castPtr outputPtr
      writeMVar (compositor ^. simulaCompositorOutput) $ Just output
      let wc = compositor ^. simulaCompositorWestonCompositor
      renderer <- westonCompositorGlRenderer wc
      eglctx <- westonGlRendererContext renderer
      egldp <- westonGlRendererDisplay renderer
      eglsurf <- westonOutputRendererSurface output
      let glctx = SimulaOpenGLContext eglctx egldp eglsurf
     
      writeMVar (compositor ^. simulaCompositorGlContext) (Just glctx)
     

instance WaylandSurface SimulaSurface where
  wsTexture = views simulaSurfaceTexture readMVar
  
  wsSize surf = V2 <$> weston_desktop_surface_get_width ws <*> weston_desktop_surface_get_height ws
    where
      ws = surf ^. simulaSurfaceWestonDesktopSurface
  
  setWsSize surf (V2 x y) =  weston_desktop_surface_set_size ws x y
    where
      ws = surf ^. simulaSurfaceWestonDesktopSurface
  
  wsPosition surf = (fmap.fmap) fromIntegral $ 
    V2 <$> weston_desktop_surface_get_position_x ws <*> weston_desktop_surface_get_position_y ws

    where
      ws = surf ^. simulaSurfaceWestonDesktopSurface
    
  wsPrepare surf = do
    comp <- readMVar (surf ^. simulaSurfaceCompositor)
    texture <- composeSurface surf (comp^.simulaCompositorOpenGlData)
    writeMVar (surf ^. simulaSurfaceTexture) (Just texture)
  
  wsSendEvent surf event = undefined


textureFromSurface :: WestonSurface -> IO TextureObject
textureFromSurface ws = do
  glState <- westonSurfaceGlState ws
  texIds <- westonGlStateTextureIds glState
  return . head $ map TextureObject texIds


composeSurface :: SimulaSurface -> OpenGLData -> IO TextureObject
composeSurface surf gld = do
  ws <- weston_desktop_surface_get_surface $ surf ^. simulaSurfaceWestonDesktopSurface
  size <- wsSize surf

  checkForErrors
  let fbo = gld ^. openGlDataSurfaceFbo
  bindFramebuffer Framebuffer $= fbo
  checkForErrors

  texture <- textureFromSurface ws
  checkForErrors

  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D texture 0
  checkForErrors
  
  paintChildren ws ws size gld
  checkForErrors

  --TODO what does this do?
--  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D (TextureObject 0) 0
--  bindFramebuffer Framebuffer $= defaultFramebufferObject
  checkForErrors
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
    paintChildren subsurface window windowSize gld
    

compositorRender :: SimulaCompositor -> IO ()
compositorRender comp = do
  surfaceMap <- readMVar (comp ^. simulaCompositorSurfaceMap)
  Just glctx <- readMVar (comp ^. simulaCompositorGlContext)
  Just output <- readMVar (comp ^. simulaCompositorOutput)


  glCtxMakeCurrent glctx
  -- set up context
  
  let surfaces = M.keys surfaceMap
  let scene = comp ^. simulaCompositorScene

  time <- getTime Realtime
  
  scenePrepareForFrame scene time
  checkForErrors
  weston_output_schedule_repaint output
  
  moveCamera
  sceneDrawFrame scene
  checkForErrors
  sceneFinishFrame scene
  checkForErrors


  emitOutputFrameSignal output
  eglSwapBuffers (glctx ^. simulaOpenGlContextEglDisplay) (glctx ^. simulaOpenGlContextEglSurface)


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

