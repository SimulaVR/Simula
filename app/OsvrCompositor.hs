import Control.Monad

import Data.Monoid

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

import Simula.Utils

Cpp.context $ Cpp.cppCtx <> C.vecCtx

Cpp.include "<motorcar.h>"
Cpp.include "<osvr_hmd.h>"
Cpp.include "<osvr_controller.h>"

main :: IO ()
main = withCArgs $ \args ->
  void $ [Cpp.block| int {
    int argc = $vec-len:args;
    char** argv = $vec-ptr:(char** args);
    
    motorcar::Scene *scene = new motorcar::Scene();

    motorcar::Compositor *compositor = motorcar::Compositor::createCompositor(argc, argv, scene, motorcar::Compositor::Type::OsvrQtWayland) ;
    scene->setCompositor(compositor);

    scene->setWindowManager( new motorcar::WindowManager(scene, compositor->defaultSeat()));

    motorcar::OpenGLContext *context = compositor->getContext();

    motorcar::Skeleton *skeleton = new motorcar::Skeleton(scene);

    float vertices[]= {
        -5.f, 0.0f, 0.0f,
        5.f, 0.0f, 0.0f,
        0.0f, -5.f, 0.0f,
        0.0f, 5.f, 0.0f,
        0.0f, 0.0f, -5.0f,
        0.0f, 0.0f, 5.f,
    };

    new motorcar::WireframeNode(vertices, 3, glm::vec3(1, 0, 0), scene, glm::translate(glm::mat4(0.01), glm::vec3(0,-1,-1)));

    std::cout << "Using OSVR HMD Display" << std::endl;
    motorcar::OsvrHMD *hmd = new motorcar::OsvrHMD(skeleton, context, scene);
    compositor->setDisplay(hmd);
    scene->addDisplay(hmd);

    motorcar::OSVRController *leftController = new motorcar::OSVRController("left", scene);
    motorcar::OSVRController *rightController = new motorcar::OSVRController("right", scene);
    leftController->setPointingDevice(new motorcar::SixDOFPointingDevice(compositor->defaultSeat(), leftController)); 
    rightController->setPointingDevice(new motorcar::SixDOFPointingDevice(compositor->defaultSeat(), rightController));

    std::cout << "Starting OSVR Compositor "<<std::endl;

    int result = compositor->start();

    delete compositor;
    delete hmd;
    delete scene;

    return result;
  } |]
