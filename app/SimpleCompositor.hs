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

  let api = WestonDesktopApi {
        apiPingTimeout = \  _ _ -> putStrLn "ping timeout",
        apiPong = \  _ _ -> putStrLn "api pong",
        apiSurfaceAdded = \  _ _ -> putStrLn "api surface added",
        apiSurfaceRemoved = \  _ _ -> putStrLn "api surface removed",
        apiCommitted = \  _ _ _ _ -> putStrLn "api committed",
        apiShowWindowMenu = \  _ _ _ _ _ -> putStrLn "api show window menu",
        apiSetParent = \  _ _ _ -> putStrLn "api set parent",
        apiMove = \  _ _ _ _ -> putStrLn "api move",
        apiResize = \  _ _ _ _ _ -> putStrLn "api resize",
        apiFullscreenRequested = \  _ _ _ _ -> putStrLn "api fullscreen requested",
        apiMaximizedRequested = \  _ _ _ -> putStrLn "api maximized requested",
        apiMinimizedRequested = \  _ _ -> putStrLn "api minimized requested"
        }

  let wc = comp ^. simulaCompositorWestonCompositor
  westonDesktopCreate wc api nullPtr
  setTimeout (compositorRender comp) 16
  putStrLn "Compositor start"
  weston_compositor_wake wc
  wl_display_run $ comp ^. simulaCompositorDisplay
  
