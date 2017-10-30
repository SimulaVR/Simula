module Simula.BaseCompositor.Weston where

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

import Simula.BaseCompositor.Compositor
import Simula.BaseCompositor.Geometry
import Simula.BaseCompositor.OpenGL
import Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.SceneGraph.Wayland
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Wayland.Output
import Simula.BaseCompositor.WindowManager
import Simula.BaseCompositor.Utils
import Simula.BaseCompositor.Types


-- data family pattern
-- instance Compositor BaseCompositor where
--   data SimulaSurface BaseCompositor = BaseCompositorSurface {
--     _baseCompositorSurfaceBase :: BaseWaylandSurface,
--     _baseCompositorSurfaceWestonDesktopSurface :: WestonDesktopSurface,
--     _baseCompositorSurfaceView :: WestonView,
--     _baseCompositorSurfaceCompositor :: MVar BaseCompositor,
--     _baseCompositorSurfaceTexture :: MVar (Maybe TextureObject),
--     _baseCompositorSurfaceStableName :: StableName (SimulaSurface BaseCompositor)
--     } deriving (Eq, Typeable)

data SimulaSurface = SimulaSurface {
  _simulaSurfaceBase                 :: BaseWaylandSurface,
  _simulaSurfaceWestonDesktopSurface :: WestonDesktopSurface,
  _simulaSurfaceView                 :: WestonView,
  --_simulaSurfaceCompositor         :: MVar SimulaCompositor,
  _simulaSurfaceBaseCompositor       :: MVar BaseCompositor,
  _simulaSurfaceTexture              :: MVar (Maybe TextureObject),
  _simulaSurfaceStableName           :: StableName SimulaSurface
  } deriving (Eq, Typeable)

instance Hashable SimulaSurface where
  hashWithSalt s = hashWithSalt s . _simulaSurfaceStableName

data BaseCompositor = BaseCompositor {
  _baseCompositorScene :: Scene,
  _baseCompositorDisplay :: Display,
  _baseCompositorWlDisplay :: WlDisplay,
  _baseCompositorWestonCompositor :: WestonCompositor,
  _baseCompositorSurfaceMap :: MVar (M.Map WestonSurface SimulaSurface),
  _baseCompositorOpenGlData :: OpenGLData,
  _baseCompositorOutput :: MVar (Maybe WestonOutput),
  _baseCompositorGlContext :: MVar (Maybe SimulaOpenGLContext),
  _baseCompositorNormalLayer :: WestonLayer
}

data SimulaSeat = SimulaSeat {
  _simulaSeatBase :: BaseSeat
  } deriving (Eq, Typeable)

data OpenGLData = OpenGLData {
  _openGlDataPpcm :: Float,
  _openGlDataTextureBlitter :: TextureBlitter,
  _openGlDataSurfaceFbo :: FramebufferObject,
  _openGlDataMousePointer :: MousePointer,
  _openGlVAO :: VertexArrayObject
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
  _textureBlitterMatrixLocation :: UniformLocation,
  _textureBlitterVertexBuffer :: BufferObject,
  _textureBlitterTextureBuffer :: BufferObject
  } deriving Eq

data MousePointer = MousePointer {
  _mousePointerProgram :: Program,
  _mousePointerSurfaceVertexCoords :: BufferObject,
  _mousePointerTextureBlitCoords :: BufferObject,
  _mousePointerPositionLocation :: AttribLocation,
  _mousePointerTexCoordLocation :: AttribLocation,
  _mousePointerPointerLocation :: UniformLocation
  } deriving Eq

makeLenses ''SimulaSurface
--makeLenses ''SimulaCompositor
makeLenses ''BaseCompositor
makeLenses ''OpenGLData
makeLenses ''SimulaOpenGLContext
makeLenses ''TextureBlitter
makeLenses ''SimulaSeat
makeLenses ''MousePointer

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

  glCtxDefaultFramebufferSize this = return $ V2 2160 1200

instance HasBaseWaylandSurface SimulaSurface where
  baseWaylandSurface = simulaSurfaceBase

newSimulaSeat :: IO SimulaSeat
newSimulaSeat = SimulaSeat <$> newBaseSeat

newSimulaSurface :: WestonDesktopSurface -> WestonView -> BaseCompositor -> WaylandSurfaceType -> IO SimulaSurface
newSimulaSurface ws view baseCompositor ty = do
  rec surface <- SimulaSurface
                 <$> newBaseWaylandSurface ty
                 <*> pure ws <*> pure view
                 <*> newMVar baseCompositor
                 <*> newMVar Nothing
                 <*> makeStableName surface
  return surface

newOpenGlData :: IO OpenGLData
newOpenGlData = OpenGLData 64 <$> newTextureBlitter <*> genObjectName <*> newMousePointer <*> genObjectName

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
    <*> genObjectName <*> genObjectName

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

  bindBuffer ArrayBuffer $= Just (tb ^. textureBlitterVertexBuffer)
  withArrayLen vertexCoordinates $ \len arrPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), arrPtr, StaticDraw)

  vertexAttribPointer vertexCoordEntry $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)

  bindBuffer ArrayBuffer $= Just (tb ^. textureBlitterTextureBuffer)
  withArrayLen textureCoordinates $ \len arrPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), arrPtr, StaticDraw)

  vertexAttribPointer textureCoordEntry $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

  uniform matrix $= transform ^. m44GLmatrix

  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  bindBuffer ArrayBuffer $= Just (tb ^. textureBlitterVertexBuffer)
  drawArrays TriangleFan 0 4
  bindBuffer ArrayBuffer $= Nothing

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

newMousePointer :: IO MousePointer
newMousePointer = do
  program <- getProgram ShaderMousePointer
  surfaceVertexCoords <- genObjectName
  let surfaceVerts = [-1, -1, 0, 1, -1, 0, 1, 1, 0, -1, 1, 0] :: [Float]
  bindBuffer ArrayBuffer $= Just surfaceVertexCoords
  withArrayLen surfaceVerts $ \len coordPtr ->
    bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)

  
  currentProgram $= Just program
  colorSampler <- get $ uniformLocation program "uColorSampler"
  uniform colorSampler $= TextureUnit 0
  currentProgram $= Nothing
  
  MousePointer
    <$> pure program
    <*> pure surfaceVertexCoords
    <*> genObjectName
    <*> get (attribLocation program "aPosition")
    <*> get (attribLocation program "aTexCoord")
    <*> get (uniformLocation program "uPointer")

drawMousePointer :: Display -> MousePointer -> V2 Float -> IO ()
drawMousePointer display mp pos = do
  bindFramebuffer Framebuffer $= defaultFramebufferObject

  textureBinding Texture2D $= Just (display ^. displayScratchColorBufferTexture)
  checkForErrors

  readBuffer $= BackBuffers

  let res = fromIntegral <$>  display ^. displaySize
  let size = TextureSize2D (res ^. _x) (res ^. _y)
  copyTexImage2D Texture2D 0 RGB' (Position 0 0) size 0
  checkForErrors

  depthMask $= Enabled
  stencilMask $= 0xff
  clearColor $= Color4 0 1 0 1
  clearDepthf $= 1
  clearStencil $= 0
  colorMask $= Color4 Enabled Enabled Enabled Enabled
  clear [ColorBuffer, DepthBuffer, StencilBuffer]
  stencilMask $= 0
  depthMask $= Disabled
  checkForErrors
  
  currentProgram $= Just (mp ^. mousePointerProgram)

  
  let aPosition = mp ^. mousePointerPositionLocation
  let aTexCoord = mp ^. mousePointerTexCoordLocation
  let surfaceCoords = mp ^. mousePointerSurfaceVertexCoords

  vertexAttribArray aPosition $= Enabled
  bindBuffer ArrayBuffer $= Just surfaceCoords
  vertexAttribPointer aPosition $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
  vertexAttribArray aTexCoord $= Enabled
  bindBuffer ArrayBuffer $= Nothing
  checkForErrors

  vps <- readMVar (display ^. displayViewpoints)
  forM_ vps $ \vp -> do
    vport <- readMVar (vp ^. viewPointViewPort)
    setViewPort vport

    let pos' = V2 (pos^._x) (res^._y.to fromIntegral - pos^._y)

    uniform (mp ^. mousePointerPointerLocation) $= (pos' ^. vector2V)
    checkForErrors
    
    vpOffset <- readMVar (vport ^. viewPortOffsetFactor)
    vpSize <- readMVar (vport ^. viewPortSizeFactor)
    let textureBlitCoords = [ vpOffset ^. _x, vpOffset ^. _y
                            , vpOffset ^. _x + vpSize ^. _x, vpOffset ^. _y
                            , vpOffset ^. _x + vpSize ^. _x, vpOffset ^. _y + vpSize  ^. _y
                            , vpOffset ^. _x, vpOffset ^. _y + vpSize ^. _y ] :: [Float]


    bindBuffer ArrayBuffer $= Just (mp ^. mousePointerTextureBlitCoords)
    withArrayLen textureBlitCoords $ \len coordPtr -> 
      bufferData ArrayBuffer $= (fromIntegral (len * sizeOf (undefined :: Float)), coordPtr, StaticDraw)


    vertexAttribPointer aTexCoord $= (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
    checkForErrors

    drawArrays TriangleFan 0 4
    checkForErrors
    bindBuffer ArrayBuffer $= Nothing

  textureBinding Texture2D $= Nothing

setTimeout :: Int -> IO () -> IO ThreadId
setTimeout ms ioOperation =
  forkIO $ do
    threadDelay (ms*1000)
    ioOperation

  
--BIG TODO: type safety for C bindings, e.g. WlSignal should encode the type of the NotifyFunc data.

instance Compositor BaseCompositor where
  startCompositor comp = do

    let wc = comp ^. baseCompositorWestonCompositor
    oldFunc <- getRepaintOutput wc
    newFunc <- createRendererRepaintOutputFunc (onRender comp oldFunc)
    setRepaintOutput wc newFunc
    weston_compositor_wake wc
    putStrLn "Compositor start"

    Just output <- readMVar (comp ^. baseCompositorOutput)
    forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000
    forkIO $ forever $ do
        let scene = comp ^. baseCompositorScene
        diffTime <- liftM2 diffTimeSpec (readMVar $ scene ^. sceneLastTimestamp) (readMVar $ scene ^. sceneCurrentTimestamp)
        let diff = fromIntegral $ toNanoSecs diffTime
        let fps = floor (10^9/diff)
        putStrLn $ "FPS: " ++ show fps
        threadDelay 1000000

    wl_display_run $ comp ^. baseCompositorWlDisplay

    where
      onRender comp oldFunc output damage = baseCompositorRender comp

  compositorDisplay = return . view baseCompositorDisplay
  compositorWlDisplay = view baseCompositorWlDisplay
  compositorOpenGLContext this = do
    Just glctx <- readMVar (this ^. baseCompositorGlContext)
    return (Some glctx)

  compositorSeat this = return (this ^. baseCompositorScene.sceneWindowManager.windowManagerDefaultSeat)
    
  compositorGetSurfaceFromResource comp resource = do
    ptr <- wlResourceData resource    
    let ws = WestonSurface (castPtr ptr)
    surface <- weston_surface_get_desktop_surface ws
    putStr "resource ptr: "
    print ptr
    simulaSurface <- M.lookup ws <$> readMVar (comp ^. baseCompositorSurfaceMap)
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
          compositor <- readMVar (simulaSurface ^. simulaSurfaceBaseCompositor)
          let wm = compositor ^. baseCompositorScene.sceneWindowManager
          wmMapSurface wm simulaSurface TopLevel
          Just output <- readMVar (compositor ^. baseCompositorOutput)
          weston_output_schedule_repaint output
      | not status && currentStatus = do
          westonSurfaceSetMapped ws False
          westonViewSetMapped view False
          compositor <- readMVar (simulaSurface ^. simulaSurfaceBaseCompositor)
          let wm = compositor ^. baseCompositorScene.sceneWindowManager
          wmUnmapSurface wm simulaSurface
      | otherwise = return ()

createSurface :: BaseCompositor -> WestonDesktopSurface  -> IO SimulaSurface
createSurface baseCompositor surface = do
  view <- weston_desktop_surface_create_view surface
  ws <- weston_desktop_surface_get_surface surface
  simulaSurface <- M.lookup ws <$> readMVar (baseCompositor ^. baseCompositorSurfaceMap)
  case simulaSurface of
    Just s -> return s
    _ -> do
      simulaSurface <- newSimulaSurface surface view baseCompositor NA
      modifyMVar' (baseCompositor ^. baseCompositorSurfaceMap) (M.insert ws simulaSurface)
  
      Just output <- readMVar (baseCompositor ^. baseCompositorOutput)
      westonViewSetOutput view output
      let layer = baseCompositor ^. baseCompositorNormalLayer
            
      weston_layer_entry_insert  (westonLayerViewList layer) (westonViewLayerEntry view)
      
      return simulaSurface

newBaseCompositor :: Scene -> Display -> IO BaseCompositor
newBaseCompositor scene display = do
  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr
  westonCompositorSetRepaintMsec wcomp 1000

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

  compositor <- BaseCompositor scene display wldp wcomp
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

  let interface = defaultWestonPointerGrabInterface {
        grabPointerFocus = onPointerFocus compositor,
        grabPointerButton = onPointerButton compositor
        }

  interfacePtr <- new interface
  weston_compositor_set_default_pointer_grab wcomp interfacePtr

  return compositor

  where
    onSurfaceCreated compositor surface  _ = do
      putStrLn "surface created"
      createSurface compositor surface
      return ()


    onSurfaceDestroyed compositor surface _ = do
      --TODO destroy surface in wm
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. baseCompositorSurfaceMap) 
      case simulaSurface of
        Just simulaSurface -> do
          modifyMVar' (compositor ^. baseCompositorSurfaceMap) (M.delete ws)
          let wm = compositor ^. baseCompositorScene.sceneWindowManager
          setSurfaceMapped simulaSurface False
          wmDestroySurface wm simulaSurface
        _ -> return ()
  
    onSurfaceCommit compositor surface x y _ = do
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. baseCompositorSurfaceMap)
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
      writeMVar (compositor ^. baseCompositorOutput) $ Just output
      let wc = compositor ^. baseCompositorWestonCompositor
      renderer <- westonCompositorGlRenderer wc
      eglctx <- westonGlRendererContext renderer
      egldp <- westonGlRendererDisplay renderer
      eglsurf <- westonOutputRendererSurface output
      let glctx = SimulaOpenGLContext eglctx egldp eglsurf
     
      writeMVar (compositor ^. baseCompositorGlContext) (Just $ SimulaOpenGLContext eglctx egldp eglsurf)

    onPointerFocus compositor grab = do
      pointer <- westonPointerFromGrab grab
      pos' <- westonPointerPosition pointer
      let pos = (`div` 256) <$> pos'
      setFocusForPointer compositor pointer pos
                     
      
    onPointerButton compositor grab time button state = do
      pointer <- westonPointerFromGrab grab
      pos' <- westonPointerPosition pointer
      let pos = (`div` 256) <$> pos'
      setFocusForPointer compositor pointer pos
      weston_pointer_send_button pointer time button state

setFocusForPointer :: BaseCompositor -> WestonPointer -> V2 Int -> IO ()
setFocusForPointer baseCompositor pointer pos = do
  ray' <- displayWorldRayAtDisplayPosition (baseCompositor ^. baseCompositorDisplay) (fromIntegral <$> pos)
  let ray = ray' & rayDir %~ negate
  inter <- nodeIntersectWithSurfaces (baseCompositor ^. baseCompositorScene) ray
  case inter of
    Nothing -> return ()
    Just rsi -> do
      Some node <- return (rsi ^. rsiSurfaceNode)
      let coords = rsi ^. rsiSurfaceCoordinates
      Some seat <- compositorSeat baseCompositor
      Some surface <- wsnSurface node
      setSeatPointerFocus seat surface coords
      sp <- seatPointer seat
      writeMVar (sp ^. pointerGlobalPosition) (fromIntegral <$> pos)
      case cast surface of
        Nothing -> return ()
        Just surface -> do
          weston_pointer_set_focus pointer (surface ^. simulaSurfaceView) (truncate (256 * coords ^. _x)) (truncate (256 * coords ^. _y))
          seat <- westonPointerSeat pointer
          kbd <- weston_seat_get_keyboard seat
          ws <- weston_desktop_surface_get_surface $ surface ^. simulaSurfaceWestonDesktopSurface
          weston_keyboard_set_focus kbd ws

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
    comp <- readMVar (surf ^. simulaSurfaceBaseCompositor)
    texture <- composeSurface surf (comp^.baseCompositorOpenGlData)
    writeMVar (surf ^. simulaSurfaceTexture) texture
  
  wsSendEvent surf event = undefined


textureFromSurface :: WestonSurface -> IO (Maybe TextureObject)
textureFromSurface ws = do
  glState <- westonSurfaceGlState ws
  case glState of
    Nothing -> return Nothing
    Just glState -> do
      texIds <- westonGlStateTextureIds glState

      let tex = TextureObject $  head texIds
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just tex
      textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )
      textureFilter Texture2D $= ( (Nearest, Nothing), Nearest )

      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
      textureBinding Texture2D $= Nothing
      return (Just tex)

composeSurface :: SimulaSurface -> OpenGLData -> IO (Maybe TextureObject)
composeSurface surf gld = do
  ws <- weston_desktop_surface_get_surface $ surf ^. simulaSurfaceWestonDesktopSurface
  size <- wsSize surf

  checkForErrors
  let fbo = gld ^. openGlDataSurfaceFbo
  bindFramebuffer Framebuffer $= fbo
  checkForErrors

  texture <- textureFromSurface ws
  case texture of
    Just texture -> do
      framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D texture 0
      checkForErrors
  
      paintChildren ws ws size gld
      checkForErrors

      -- needed for textures to properly render
      framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D (TextureObject 0) 0
      bindFramebuffer Framebuffer $= defaultFramebufferObject
      checkForErrors
    _ -> return ()
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
      -- if we're in this code, we should have a texture
      Just tex <- textureFromSurface subsurface
      let geo = Rect p (fromIntegral <$> subSize)
      windowInverted <- westonSurfaceIsYInverted window
      subsurfaceInverted <- westonSurfaceIsYInverted subsurface
      blitterDrawTexture (gld ^. openGlDataTextureBlitter) tex geo windowSize 0 windowInverted subsurfaceInverted
    paintChildren subsurface window windowSize gld

--BUG TODO: need an actual wl_shell

baseCompositorRender :: BaseCompositor -> IO ()
baseCompositorRender comp = do
  surfaceMap <- readMVar (comp ^. baseCompositorSurfaceMap)
  Just glctx <- readMVar (comp ^. baseCompositorGlContext)
  Just output <- readMVar (comp ^. baseCompositorOutput)

  glCtxMakeCurrent glctx
  bindVertexArrayObject $= Just (comp ^. baseCompositorOpenGlData.openGlVAO)

  -- set up context

  let surfaces = M.keys surfaceMap
  let scene  = comp ^. baseCompositorScene
  let simDisplay = comp ^. baseCompositorDisplay

  time <- getTime Realtime
  scenePrepareForFrame scene time
  checkForErrors

  weston_output_schedule_repaint output
  checkPosesAndDraw simDisplay scene comp glctx output
  bindVertexArrayObject $= Nothing



checkPosesAndDraw disp scene comp glctx out = do
    Some seat <- compositorSeat comp
    sceneDrawFrame scene
    checkForErrors

    drawScene seat comp glctx out

    sceneFinishFrame scene
    checkForErrors
        

drawScene seat comp glctx out = do
    pointer <- seatPointer seat
    pos <- readMVar (pointer ^. pointerGlobalPosition)
    drawMousePointer (comp ^. baseCompositorDisplay) (comp ^. baseCompositorOpenGlData.openGlDataMousePointer) pos

    emitOutputFrameSignal out
    eglSwapBuffers (glctx ^. simulaOpenGlContextEglDisplay) (glctx ^. simulaOpenGlContextEglSurface)

