import Control.Lens
import Data.IORef
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

data DummySeat = DummySeat
  deriving (Eq, Typeable)
instance Seat DummySeat


foreign import ccall "dynamic" fromShellInit :: FunPtr (WestonCompositor -> CInt -> Ptr CChar -> IO ()) -> WestonCompositor -> CInt -> Ptr CChar -> IO ()

main :: IO ()
main = do
  rec -- order is important
    comp <- newSimulaCompositor scene disp
    Just glctx <- readIORef (comp ^. simulaCompositorGlContext)
    scene <- Scene <$> newBaseNode Nothing identity
           <*> newIORef 0 <*> newIORef 0
           <*> pure wm <*> newIORef (Some comp) <*> newIORef [] <*> newIORef Nothing
    disp <- newDisplay glctx (V2 2000 2000) (V2 2000 2000) scene (translate (V3 0 0.8 1.25))
    vp <- newViewPoint 0.01 100 disp disp (translate (V3 0 0 0.1)) (V4 0 0 1 1) (V3 0 0 0)
    modifyIORef' (disp ^. displayViewpoints) (vp:)
    modifyIORef' (scene ^. sceneDisplays) (disp:)
    
    wm <- newWindowManager scene DummySeat

 
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

