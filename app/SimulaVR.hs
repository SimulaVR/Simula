import Control.Exception
import Control.Lens
import Control.Concurrent.MVar
import Data.Typeable
import Data.Monoid ((<>))
-- optparse-applicative
import Options.Applicative

--import Simula.BaseCompositor.BaseCompositor
import Simula.BaseCompositor.Compositor
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Weston
import Simula.BaseCompositor.WindowManager
import Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Utils
import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Foreign.C
import Linear
import Graphics.Rendering.OpenGL hiding (translate, scale, rotate)

data EngineOptions = EngineOptions
  { _verbose :: Bool
  , _waitHMD :: Bool
  } deriving (Eq, Show)

makeLenses ''EngineOptions

main :: IO ()
main = do
  let dpRot = axisAngle (V3 1 0 0) (radians (negate 25))
  let dpTf = translate (V3 0 0.8 1.25) !*! m33_to_m44 (fromQuaternion dpRot)
  eOpts <- execParser engineOptionsWrapper
  seat <- newSimulaSeat
  rec -- order is important
    comp <- newBaseCompositor scene disp (eOpts ^. waitHMD)
    Just glctx <- readMVar (comp ^. baseCompositorGlContext)
    scene <- Scene <$> newBaseNode scene Nothing identity
           <*> newMVar 0 <*> newMVar 0
           <*> pure wm <*> newMVar (Some comp) <*> newMVar [] <*> newMVar Nothing
    disp <- newDisplay glctx (V2 1280 720) (V2 0.325 0.1) scene dpTf
    vp <- newViewPoint 0.01 100 disp disp (translate (V3 0 0 0.1)) (V4 0 0 1 1) (V3 0 0 0)
    modifyMVar' (disp ^. displayViewpoints) (vp:)
    modifyMVar' (scene ^. sceneDisplays) (disp:)

    wm <- newWindowManager scene seat
  startCompositor comp

engineOptsParser :: Parser EngineOptions
engineOptsParser = EngineOptions
  <$> switch
    (long "verbose"
      <> short 'v'
      <> help "The most messages you can get")
  <*> switch
    (long "waitHMD"
      <> short 'w'
      <> help "Wait indefinitely for the HMD to be recognized")


engineOptionsWrapper :: ParserInfo EngineOptions
engineOptionsWrapper = info (engineOptsParser <**> helper)
  (fullDesc
    <> progDesc ""
    <> header "base-compositor - basic engine for VR Desktop"
  )
