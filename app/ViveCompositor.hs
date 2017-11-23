import Control.Exception
import Control.Lens
import Control.Concurrent.MVar
import Data.Typeable

import Simula.BaseCompositor.Compositor
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Weston
import Simula.BaseCompositor.WindowManager
import Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Utils

import Simula.ViveCompositor.ViveCompositor

import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Foreign.C
import Linear
import Graphics.Rendering.OpenGL hiding (translate, scale, rotate)

main :: IO ()
main = do
  viveComp <- newViveCompositor 
  startCompositor viveComp
