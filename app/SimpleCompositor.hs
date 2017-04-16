import Control.Monad

import Data.Monoid

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp
import System.Environment

import Simula.Compositor
import Simula.Compositor.Types
import Simula.Context

Cpp.context simulaCtx

Cpp.include "<motorcar.h>"

--todo: put all verbatims in one location
Cpp.verbatim "typedef motorcar::Scene Scene;"
Cpp.verbatim "typedef motorcar::Compositor Compositor;"
Cpp.verbatim "typedef motorcar::WindowManager WindowManager;"
Cpp.verbatim "typedef motorcar::OpenGLContext OpenGLContext;"
Cpp.verbatim "typedef motorcar::Display Display;"
Cpp.verbatim "typedef motorcar::ViewPoint ViewPoint;"


main :: IO ()
main = do
  args <- getArgs
  
  scene <- newScene
  compositor <- createCompositor args scene
  setSceneCompositor scene compositor

  seat <- getCompositorDefaultSeat compositor
  
  wm <- newWindowManager scene seat
  setSceneWindowManager scene wm

  glctx <- getCompositorContext compositor



  skeleton <- newSkeleton scene

  putStrLn "Using Default Display"

  -- hack because glm stuff is a headache
  let (OpenGLContext ctxPtr) = glctx
  let (Scene scenePtr) = scene  
  display <- Display <$> [Cpp.exp| Display* {
    new motorcar::Display($(OpenGLContext* ctxPtr), glm::vec2(0.325f, 0.1f), $(Scene* scenePtr), glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.8f, 1.25f))
    * glm::rotate(glm::mat4(1.0f), glm::radians(-25.0f), glm::vec3(1.0f, 0.0f, 0.0f)))
    } |]


  let camToDisplayDistance = 0.1
  let (Display disPtr) = display
  viewpoint <- ViewPoint <$> [Cpp.exp| ViewPoint* {
    new motorcar::ViewPoint( .01f, 100.0f, $(Display* disPtr), $(Display* disPtr), glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.0f, $(float camToDisplayDistance))))
  } |]

  addDisplayViewpoint display viewpoint

  setCompositorDisplay compositor display
  addSceneDisplay scene display

  putStrLn "Starting Compositor"
  startCompositor compositor
  deleteScene scene

