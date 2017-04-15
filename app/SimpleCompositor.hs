import Control.Monad

import Data.Monoid

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

import Simula.Utils

Cpp.context $ Cpp.cppCtx <> C.vecCtx

Cpp.include "<motorcar.h>"
  

main :: IO ()
main = withCArgs $ \args -> 
  void $ [Cpp.block| int {
    int argc = $vec-len:args;
    char** argv = $vec-ptr:(char** args);
    motorcar::Scene *scene = new motorcar::Scene();

    motorcar::Compositor *compositor = motorcar::Compositor::createCompositor(argc, argv, scene) ;
    scene->setCompositor(compositor);

    scene->setWindowManager( new motorcar::WindowManager(scene, compositor->defaultSeat()));

    motorcar::OpenGLContext *context = compositor->getContext();

    motorcar::Skeleton *skeleton = new motorcar::Skeleton(scene);

   
    std::cout << "Using Default Display" << std::endl;
    float camToDisplayDistance = 0.1f;
    motorcar::Display *display = new motorcar::Display(context, glm::vec2(0.325f, 0.1f), scene, glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.8f, 1.25f))
                                                       * glm::rotate(glm::mat4(1.0f), glm::radians(-25.0f), glm::vec3(1.0f, 0.0f, 0.0f)));
    display->addViewpoint(new motorcar::ViewPoint( .01f, 100.0f, display, display, glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.0f, camToDisplayDistance))));
    compositor->setDisplay(display);
    scene->addDisplay(compositor->display());


    std::cout << "Starting Compositor "<<std::endl;


    int result = compositor->start();

    delete scene;
    return result;
  } |]
  
