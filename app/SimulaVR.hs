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

import Simula.ViveCompositor.ViveCompositor

import Simula.WestonDesktop
import Simula.Weston
import Simula.WaylandServer
import Foreign
import Foreign.C
import Linear
import Graphics.Rendering.OpenGL hiding (translate, scale, rotate)

data EngineOptions = EngineOptions
  { _verbose :: Bool
  } deriving (Eq, Show)

makeLenses ''EngineOptions

main :: IO ()
main = do
  eOpts <- execParser engineOptionsWrapper
  viveComp <- newViveCompositor (eOpts ^. verbose)
  startCompositor viveComp

engineOptsParser :: Parser EngineOptions
engineOptsParser = EngineOptions
  <$> switch
    (long "verbose"
      <> short 'v'
      <> help "The most messages you can get")

engineOptionsWrapper :: ParserInfo EngineOptions
engineOptionsWrapper = info (engineOptsParser <**> helper)
  (fullDesc
    <> progDesc ""
    <> header "base-compositor - basic engine for VR Desktop"
  )
