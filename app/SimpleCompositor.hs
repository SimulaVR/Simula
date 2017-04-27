import Control.Lens
import Simula.NewCompositor.Weston
import Simula.NewCompositor.SceneGraph
import Simula.Weston
import Simula.WaylandServer

main :: IO ()
main = do
  comp <- newSimulaCompositor undefined
  weston_compositor_wake $ comp ^. simulaCompositorWestonCompositor
  wl_display_run $ comp ^. simulaCompositorDisplay
  
