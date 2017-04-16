module Simula.Compositor where

import Control.Monad.IO.Class
import Foreign
import Foreign.C

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
Cpp.verbatim "typedef motorcar::ViewPoint ViewPoint;"
Cpp.verbatim "typedef motorcar::Seat Seat;"
Cpp.verbatim "typedef motorcar::Skeleton Skeleton;"

-- Scene

newScene :: IO Scene
newScene = Scene <$> [Cpp.exp| Scene* { new motorcar::Scene() } |]

setSceneCompositor :: Scene -> Compositor -> IO ()
setSceneCompositor (Scene scene) (Compositor comp) = [Cpp.exp| void {
     $(Scene* scene)->setCompositor($(Compositor* comp))
  } |]

setSceneWindowManager :: Scene -> WindowManager -> IO ()
setSceneWindowManager (Scene scene) (WindowManager wm) = [Cpp.exp| void {
    $(Scene* scene)->setWindowManager($(WindowManager* wm))
  } |]

addSceneDisplay :: Scene -> Display -> IO ()
addSceneDisplay (Scene scene) (Display disp) = [Cpp.exp| void {
    $(Scene* scene)->addDisplay($(Display* disp))
  } |]

deleteScene :: Scene -> IO ()
deleteScene (Scene scene) = [Cpp.exp| void { delete $(Scene* scene) }|]

-- Compositor

-- requires argc hack due to createCompositor expecting a lvalue
createCompositor :: [String] -> Scene -> IO Compositor
createCompositor args (Scene scene) = withCStringVector args $ \args ->
  Compositor <$> [Cpp.block| Compositor* {
    int argc = $vec-len:args;
    return Compositor::createCompositor(argc, $vec-ptr:(char** args), $(Scene* scene));
  } |]

getCompositorDefaultSeat :: Compositor -> IO Seat
getCompositorDefaultSeat (Compositor comp) =
  Seat <$> [Cpp.exp| Seat* {
    $(Compositor* comp)->defaultSeat()
  } |]

getCompositorContext :: Compositor -> IO OpenGLContext
getCompositorContext (Compositor comp) =
  OpenGLContext <$> [Cpp.exp| OpenGLContext* {
    $(Compositor* comp)->getContext()
  } |]

getCompositorDisplay :: Compositor -> IO Display
getCompositorDisplay (Compositor comp) =
  Display <$> [Cpp.exp| Display* {
    $(Compositor* comp)->display()
  } |]

setCompositorDisplay :: Compositor -> Display -> IO ()
setCompositorDisplay (Compositor comp) (Display disp) = [Cpp.exp| void {
  $(Compositor* comp)->setDisplay($(Display* disp))
  } |]


startCompositor :: Compositor -> IO CInt
startCompositor (Compositor comp) = [Cpp.exp| int { $(Compositor* comp)->start() } |]

-- Window manager

newWindowManager :: Scene -> Seat -> IO WindowManager
newWindowManager (Scene scene) (Seat seat) =
  WindowManager <$> [Cpp.exp| WindowManager* {
    new motorcar::WindowManager($(Scene* scene), $(Seat* seat))
  } |]

-- Skeleton

newSkeleton :: Scene -> IO Skeleton
newSkeleton (Scene scene) =
  Skeleton <$> [Cpp.exp| Skeleton* {
    new motorcar::Skeleton($(Scene* scene))
  } |]


-- Display
addDisplayViewpoint :: Display -> ViewPoint -> IO ()
addDisplayViewpoint (Display disp) (ViewPoint vp) = [Cpp.exp| void {
   $(Display* disp)->addViewpoint($(ViewPoint* vp))
  } |]
