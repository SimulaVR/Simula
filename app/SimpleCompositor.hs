import Control.Lens
import Control.Concurrent.MVar
import Data.Typeable

import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Weston
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types
import Simula.NewCompositor.Utils
import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Foreign.C
import Linear

import Graphics.Rendering.OpenGL hiding (translate, scale, rotate)
import System.Posix.DynamicLinker



foreign import ccall "dynamic" fromShellInit :: FunPtr (WestonCompositor -> CInt -> Ptr CChar -> IO ()) -> WestonCompositor -> CInt -> Ptr CChar -> IO ()

main :: IO ()
main = do
  seat <- newSimulaSeat
  let dpRot = axisAngle (V3 1 0 0) (radians (negate 25))
  let dpTf = translate (V3 0 0.8 1.25) !*! m33_to_m44 (fromQuaternion dpRot)
  rec -- order is important
    comp <- newSimulaCompositor scene disp
    Just glctx <- readMVar (comp ^. simulaCompositorGlContext)
    scene <- Scene <$> newBaseNode scene Nothing identity
           <*> newMVar 0 <*> newMVar 0
           <*> pure wm <*> newMVar (Some comp) <*> newMVar [] <*> newMVar Nothing
    disp <- newDisplay glctx (V2 1280 720) (V2 0.325 0.1) scene dpTf
    vp <- newViewPoint 0.01 100 disp disp (translate (V3 0 0 0.1)) (V4 0 0 1 1) (V3 0 0 0)
    modifyMVar' (disp ^. displayViewpoints) (vp:)
    modifyMVar' (scene ^. sceneDisplays) (disp:)
    
    wm <- newWindowManager scene seat
 
  let wc = comp ^. simulaCompositorWestonCompositor

  oldFunc <- getRepaintOutput wc
  newFunc <- createRendererRepaintOutputFunc (onRender comp oldFunc)
  setRepaintOutput wc newFunc

  
  putStrLn "Compositor start"
  weston_compositor_wake wc
  wl_display_run $ comp ^. simulaCompositorWlDisplay

  where
    onRender comp oldFunc output damage = do
      compositorRender comp
      putStrLn "rendered"

