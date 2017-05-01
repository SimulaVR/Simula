import Control.Lens
import Data.IORef
import Data.Typeable

import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Weston
import Simula.NewCompositor.WindowManager
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.Types
import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Linear


data DummySeat = DummySeat
  deriving (Eq, Typeable)
instance Seat DummySeat


main :: IO ()
main = do
  rec -- order is important
    comp <- newSimulaCompositor scene
    scene <- Scene <$> newBaseNode Nothing identity
           <*> newIORef 0 <*> newIORef 0
           <*> pure wm <*> newIORef (Some comp) <*> newIORef [] <*> newIORef Nothing
    wm <- newWindowManager scene DummySeat


  let wc = comp ^. simulaCompositorWestonCompositor


  setTimeout (compositorRender comp) 16
  putStrLn "Compositor start"
  weston_compositor_wake wc
  wl_display_run $ comp ^. simulaCompositorDisplay
  
