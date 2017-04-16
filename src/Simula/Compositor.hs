module Simula.Compositor where

import Control.Monad.IO.Class
import Foreign

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

import Simula.Context
import Simula.Compositor.Types
import Simula.Utils

Cpp.context simulaCtx

Cpp.include "<motorcar.h>"

Cpp.verbatim "typedef motorcar::Scene Scene;"
Cpp.verbatim "typedef motorcar::Compositor Compositor;"
Cpp.verbatim "typedef motorcar::WindowManager WindowManager;"
Cpp.verbatim "typedef motorcar::OpenGLContext OpenGLContext;"
Cpp.verbatim "typedef motorcar::Display Display;"

newScene :: IO Scene
newScene = Scene <$> [Cpp.exp| Scene* { new motorcar::Scene(); } |]

setSceneCompositor :: Scene -> Compositor -> IO ()
setSceneCompositor (Scene scene) (Compositor comp) = [Cpp.exp| void {
     $(Scene* scene)->setCompositor($(Compositor* comp))
  } |]

setSceneWindowManager :: Scene -> WindowManager -> IO ()
setSceneWindowManager (Scene scene) (WindowManager wm) = [Cpp.exp| void {
    $(Scene* scene)->setWindowManager($(WindowManager* wm))
  } |]


createCompositor :: [String] -> Scene -> IO Compositor
createCompositor args (Scene scene) = withCStringVector args $ \args ->
  Compositor <$> [Cpp.exp| Compositor* {
    Compositor::createCompositor($vec-len:args, $vec-ptr:(char** args), $(Scene* scene))
  } |]

getCompositorDefaultSeat :: Compositor -> IO Seat
getCompositorDefaultSeat (Compositor comp) =
  Seat <$> [Cpp.exp| Seat* {
    $(Compositor* comp)->defaultSeat()
  } |]

newWindowManager :: Scene -> Seat -> IO WindowManager
newWindowManager (Scene scene) (Seat seat) =
  WindowManager <$> [Cpp.exp| WindowManager* {
    new motorcar::WindowManager($(Scene* scene), $(Seat* seat))
  } |]
