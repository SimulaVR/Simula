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
  seat <- newSimulaSeat
  let dpRot = axisAngle (V3 1 0 0) (radians (negate 25))
  let dpTf = translate (V3 0 0.8 1.25) !*! m33_to_m44 (fromQuaternion dpRot)
  rec -- order is important
    viveComp <- newViveCompositor scene disp
    let baseComp = (_viveCompositorBaseCompositor viveComp)
    Just glctx <- readMVar (baseComp ^. baseCompositorGlContext)
    scene <- Scene <$> newBaseNode scene Nothing identity
           <*> newMVar 0 <*> newMVar 0
           <*> pure wm <*> newMVar (Some baseComp) <*> newMVar [] <*> newMVar Nothing
    disp <- newDisplay glctx (V2 1512 1680) (V2 0.325 0.1) scene dpTf
    vp <- newViewPoint 0.01 100 disp disp (translate (V3 0 0 0.1)) (V4 0 0 1 1) (V3 0 0 0)
    modifyMVar' (disp ^. displayViewpoints) (vp:)
    modifyMVar' (scene ^. sceneDisplays) (disp:)

    wm <- newWindowManager scene seat
  startCompositor viveComp
