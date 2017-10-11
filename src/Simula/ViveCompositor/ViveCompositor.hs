module Simula.ViveCompositor.ViveCompositor where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Control.Concurrent.MVar
import Data.Hashable
import Data.Word
import Data.Typeable
import Data.Maybe
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
import Simula.BaseCompositor.Weston hiding (moveCamera)

import Simula.OSVR
import Simula.ViveCompositor.OSVR

-- data family pattern
-- instance Compositor ViveCompositor where
--   data SimulaSurface ViveCompositor = ViveCompositorSurface {
--     _viveCompositorSurfaceBase :: BaseWaylandSurface,
--     _viveCompositorSurfaceSurface :: WestonDesktopSurface,
--     _viveCompositorSurfaceView :: WestonView,
--     _viveCompositorSurfaceCompositor :: MVar ViveCompositor,
--     _viveCompositorSurfaceTexture :: MVar (Maybe TextureObject),
--     _viveCompositorSurfaceStableName :: StableName (SimulaSurface ViveCompositor)
--   } deriving (Eq, Typeable)

data ViveCompositor = ViveCompositor {
  _viveCompositorBaseCompositor :: BaseCompositor,
  _viveCompositorOSVR :: SimulaOSVRClient
}

makeLenses ''ViveCompositor

newViveCompositor :: Scene -> Display -> IO ViveCompositor
newViveCompositor scene display = do
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

  baseCompositor <- BaseCompositor scene display wldp wcomp
                <$> newMVar M.empty <*> newOpenGlData
                <*> newMVar Nothing <*> newMVar Nothing
                <*> pure mainLayer
                <*> initSimulaOsvrClient

  windowedApi <- weston_windowed_output_get_api wcomp

  let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
  outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi baseCompositor)
  addListenerToSignal outputPendingSignal outputPendingPtr

  let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
  outputCreatedPtr <- createNotifyFuncPtr (onOutputCreated baseCompositor)
  addListenerToSignal outputCreatedSignal outputCreatedPtr
 
  westonWindowedOutputCreate windowedApi wcomp "X"


  let api = defaultWestonDesktopApi {
        apiSurfaceAdded = onSurfaceCreated baseCompositor,
        apiSurfaceRemoved = onSurfaceDestroyed baseCompositor,
        apiCommitted = onSurfaceCommit baseCompositor
        }

  
  westonDesktopCreate wcomp api nullPtr

  let interface = defaultWestonPointerGrabInterface {
        grabPointerFocus = onPointerFocus baseCompositor,
        grabPointerButton = onPointerButton baseCompositor
        }

  interfacePtr <- new interface
  weston_compositor_set_default_pointer_grab wcomp interfacePtr

  -- setupHeadTracking (_baseCompositorOSVR baseCompositor) initSimulaOSVRClient
  --   >> setupLeftHandTracking (_baseCompositorOSVR baseCompositor)
  --   >> setupRightHandTracking (_baseCompositorOSVR baseCompositor)

  osvrClient <- initSimulaOsvrClient
  setupHeadTracking osvrClient
  setupLeftHandTracking osvrClient
  setupRightHandTracking osvrClient

  return (ViveCompositor baseCompositor osvrClient)

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
     
      writeMVar (compositor ^. baseCompositorGlContext) (Just glctx)

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

moveCamera :: Display -> PoseTracker -> IO Display
moveCamera d p = return d

drawLeftHand :: PoseTracker -> IO ()
drawLeftHand p = return ()

drawRightHand :: PoseTracker -> IO ()
drawRightHand p = return ()

viveCompositorRender :: ViveCompositor -> IO ()
viveCompositorRender viveComp = do
  let comp = viveComp ^. viveCompositorBaseCompositor
  surfaceMap <- readMVar (comp ^. baseCompositorSurfaceMap)
  Just glctx <- readMVar (comp ^. baseCompositorGlContext)
  Just output <- readMVar (comp ^. baseCompositorOutput)

  glCtxMakeCurrent glctx
  -- set up context

  let surfaces = M.keys surfaceMap
  let scene  = comp ^. baseCompositorScene
  let simDisplay = comp ^. baseCompositorDisplay

  let osvrCtx = viveComp ^. viveCompositorOSVR.simulaOsvrContext
  let osvrDisplay = viveComp ^. viveCompositorOSVR.simulaOsvrDisplay

  time <- getTime Realtime
  scenePrepareForFrame scene time
  checkForErrors
  osvrClientUpdate osvrCtx

  case osvrDisplay of
    Nothing -> do -- ioError $ userError "Could not initialize display in OSVR"
      putStrLn "[INFO] no OSVR display is connected"

      -- We can still render to wayland though
      weston_output_schedule_repaint output
      headP <- osvrGetHeadPose osvrCtx
      when (isJust headP) (moveCamera simDisplay (fromJust headP) >>= \d -> writeMVar (scene ^. sceneDisplays) [d])

      osvrGetLeftHandPose osvrCtx
      osvrGetRightHandPose osvrCtx

      sceneDrawFrame scene
      checkForErrors

      Some seat <- compositorSeat comp
      pointer <- seatPointer seat
      pos <- readMVar (pointer ^. pointerGlobalPosition)
      drawMousePointer (comp ^. baseCompositorDisplay) (comp ^. baseCompositorOpenGlData.openGlDataMousePointer) pos

      emitOutputFrameSignal output
      eglSwapBuffers (glctx ^. simulaOpenGlContextEglDisplay) (glctx ^. simulaOpenGlContextEglSurface)

      sceneFinishFrame scene
      checkForErrors

    Just display -> do
      -- TODO: make the Head Mounted Display work at all
      (value, viewers) <- osvrClientGetNumViewers display
      case value of
          ReturnSuccess -> do
            putStrLn $ "[INFO] viewer count is " ++ show viewers
            weston_output_schedule_repaint output

            sceneDrawFrame scene
            checkForErrors

            when (viewers > 0) $ do
                forM_ [0..(fromIntegral viewers - 1)] $ \viewer -> do
                    -- TODO: Check for Errors for ReturnSuccess
                    (ReturnSuccess, eyes) <- osvrClientGetNumEyesForViewer display (fromIntegral viewer)
                    if eyes < 1 then error "Well, we got no eyes!"
                    else do
                        forM_ [0..(eyes - 1)] $ \eye -> do
                          viewMat <- osvrClientGetViewerEyeViewMatrixf' display viewer (fromIntegral eye)

                          (vp:_) <- readMVar (comp ^. baseCompositorDisplay.displayViewpoints)
                          --TODO test
                          setNodeWorldTransform vp viewMat
                          viewPointUpdateViewMatrix vp

                          -- TODO: Check for Errors for ReturnSuccess
                          (ReturnSuccess, osvrSurfaces) <-
                            osvrClientGetNumSurfacesForViewerEye display viewer (fromIntegral eye)

                          forM_ [0..osvrSurfaces] $ \osvrSurface -> do
                            projMat <- osvrClientGetViewerEyeSurfaceProjectionMatrixf' display viewer (fromIntegral eye) osvrSurface (vp^.viewPointNear) (vp^.viewPointFar)

                            viewPointOverrideProjectionMatrix vp projMat
                            Some seat <- compositorSeat comp
                            pointer <- seatPointer seat
                            pos <- readMVar (pointer ^. pointerGlobalPosition)
                            drawMousePointer (comp ^. baseCompositorDisplay) (comp ^. baseCompositorOpenGlData.openGlDataMousePointer) pos

                            emitOutputFrameSignal output
                            eglSwapBuffers (glctx ^. simulaOpenGlContextEglDisplay) (glctx ^. simulaOpenGlContextEglSurface)
            sceneFinishFrame scene
            checkForErrors

instance Compositor ViveCompositor where
  startCompositor viveComp = do
    let comp = viveComp ^. viveCompositorBaseCompositor
    let wc = comp ^. baseCompositorWestonCompositor
    oldFunc <- getRepaintOutput wc
    newFunc <- createRendererRepaintOutputFunc (onRender viveComp oldFunc)
    setRepaintOutput wc newFunc
    weston_compositor_wake wc
    putStrLn "Compositor start"
    wl_display_run $ comp ^. baseCompositorWlDisplay

    where
      onRender viveComp oldFunc output damage = viveCompositorRender viveComp

  compositorDisplay viveComp = do
    return (viveComp ^. viveCompositorBaseCompositor . baseCompositorDisplay)

  compositorWlDisplay viveComp =
    viveComp ^. viveCompositorBaseCompositor . baseCompositorWlDisplay

  compositorOpenGLContext viveComp = do
    let baseComp = viveComp ^. viveCompositorBaseCompositor
    Just glctx <- readMVar (baseComp ^. baseCompositorGlContext)
    return (Some glctx)

  compositorSeat viveComp = return (viveComp ^. viveCompositorBaseCompositor . baseCompositorScene.sceneWindowManager.windowManagerDefaultSeat)
    
  compositorGetSurfaceFromResource viveComp resource = do
    let comp = (viveComp ^. viveCompositorBaseCompositor)
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
