module Simula.BaseCompositor.WindowManager where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.Typeable
import Foreign
import Foreign.C
import Linear
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Wayland.Output
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Utils

data WindowManager
instance Eq WindowManager
