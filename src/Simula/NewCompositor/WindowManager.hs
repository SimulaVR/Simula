module Simula.NewCompositor.WindowManager where

import Data.IORef

import Simula.NewCompositor.Event
import Simula.NewCompositor.SceneGraph
import Simula.NewCompositor.SceneGraph.Wayland
import Simula.NewCompositor.Wayland.Input
import Simula.NewCompositor.Wayland.Output
import Simula.NewCompositor.Types


class WindowManager a where
  wmCreateSurface :: WaylandSurface ws => a -> ws -> IO (Some WaylandSurfaceNode)
  wmMapSurface :: WaylandSurface ws => a -> ws -> WaylandSurfaceType -> IO (Some WaylandSurfaceNode)
  wmUnmapSurface :: WaylandSurface ws => a -> ws -> IO ()
  wmSendEvent :: Event ev => a -> ev -> IO ()

  wmSurfaceNode :: WaylandSurface ws => a -> ws -> IO (Some WaylandSurfaceNode)

  wmScene :: a -> IO Scene
  setWmScene :: a -> Scene -> IO ()

  wmDefaultSeat :: a -> IO (Some Seat)
  setWmDefaultSeat :: Seat b => a -> b -> IO ()

  wmEnsureKeyboardFocusIsInvalid :: WaylandSurface ws => a -> ws -> IO ()

{-
WindowManager::WindowManager(Scene *scene, Seat *defaultSeat)
    :m_numSurfacesMapped(0)
    ,m_scene(scene)
    ,m_defaultSeat(defaultSeat)
    ,m_shell(new Shell(scene))
{

}

WindowManager::~WindowManager()
{
    delete m_defaultSeat;
    delete m_shell;
}

WaylandSurfaceNode *WindowManager::createSurface(WaylandSurface *surface)
{

    WaylandSurfaceNode *surfaceNode = this->getSurfaceNode(surface);
    if(surfaceNode==NULL){
        if(surface->isMotorcarSurface()){
            surfaceNode = new MotorcarSurfaceNode(surface, this->scene());
        }else{
            surfaceNode = new WaylandSurfaceNode(surface, this->scene());
        }
        std::cout << "allocating surface node " << surfaceNode << " for surface " << surface <<std::endl;
        m_surfaceMap.insert(std::pair<WaylandSurface *, motorcar::WaylandSurfaceNode *>(surface, surfaceNode));
    }

    surfaceNode->setSurface(surface);
    return surfaceNode;
}

void WindowManager::destroySurface(WaylandSurface *surface)
{
    std::cout << "destroying surface " << surface <<std::endl;
    WaylandSurfaceNode *surfaceNode = this->getSurfaceNode(surface);
    if(surfaceNode != NULL){

        std::vector<motorcar::SceneGraphNode *> subtreeNodes = surfaceNode->nodesInSubtree();
        for(motorcar::SceneGraphNode *node : subtreeNodes){
            motorcar::WaylandSurfaceNode *subtreeSurfaceNode = dynamic_cast<motorcar::WaylandSurfaceNode *>(node);
            if(subtreeSurfaceNode != NULL){
                WaylandSurface *subtreeSurface = subtreeSurfaceNode->surface();

                    std::map<WaylandSurface *, motorcar::WaylandSurfaceNode *>::iterator it = m_surfaceMap.find(subtreeSurface);
                    if (it != m_surfaceMap.end()){
                        std::cout << "nulling surfaceNode pointer: " << it->second  << " in surface map" <<std::endl;
                        it->second = NULL;
                    }

            }
        }

        m_surfaceMap.erase (surface);

        std::cout << "attempting to delete surfaceNode pointer " << surfaceNode <<std::endl;

        if(surfaceNode == m_defaultSeat->pointer()->cursorNode()){
            m_defaultSeat->pointer()->setCursorNode(NULL);
        }

        ensureKeyboardFocusIsValid(surface);
        delete surfaceNode;



    }
}


WaylandSurfaceNode *WindowManager::mapSurface(motorcar::WaylandSurface *surface, WaylandSurface::SurfaceType surfaceType)
{

    surface->setType(surfaceType);

    WaylandSurfaceNode *surfaceNode = createSurface(surface);



    int type = static_cast<int>(surfaceType);
    float popupZOffset = 0.05f;

    float zOffset = -1.5f;
    float thetaOffset = -30.0f;


    if(surface->isMotorcarSurface()){
         MotorcarSurfaceNode *mcsn = static_cast<MotorcarSurfaceNode *>(surfaceNode);
         std::cout << "mapping motorcar surface ";
         if(surface->clippingMode() == WaylandSurface::ClippingMode::CUBOID){
             std::cout << "with cuboid clipping mode" <<std::endl;
             mcsn->requestSize3D(glm::vec3(0.5f));
         }else if(surface->clippingMode() == WaylandSurface::ClippingMode::PORTAL){
              std::cout << "with portal clipping mode" <<std::endl;
             mcsn->requestSize3D(glm::vec3(0.7f, 0.5f, 0.0f));
         }

         if(surface->depthCompositingEnabled()){
             surface->setSize(this->scene()->compositor()->display()->size() * glm::ivec2(1, 2));
         }else{
             surface->setSize(this->scene()->compositor()->display()->size());
         }
    }


    if(type == WaylandSurface::SurfaceType::TOPLEVEL){
         std::cout << "mapping top level surface" << std::endl;
        surfaceNode->setParentNode(this->scene());
//        surfaceNode->setTransform(glm::mat4(1)
//                      //  * glm::rotate(glm::mat4(1), glm::radians(-90.f), glm::vec3(0, 1, 0))
//                        * glm::translate(glm::mat4(1), glm::vec3(0, 0 ,1.25f))
//                       // * glm::rotate(glm::mat4(1), glm::radians((-1 +  m_numSurfacesMapped % 3) * 30.f), glm::vec3(0, -1, 0))
//                    //* glm::rotate(glm::mat4(1),  glm::radians((-1 + m_numSurfacesMapped / 3) * 30.f), glm::vec3(-1, 0, 0))
//                    * glm::translate(glm::mat4(1), glm::vec3(0,0.0,-1.5f))
//                    * glm::mat4(1));
        surfaceNode->setTransform(glm::translate(glm::mat4(1), glm::vec3(0.0f, 0.0f ,1.0f)) *
                            glm::rotate(glm::mat4(1.0f), glm::radians((m_numSurfacesMapped - 1) * thetaOffset), glm::vec3(0.0f, 1.0f ,0.0f)) *
                                    glm::translate(glm::mat4(1.0f), glm::vec3(0.0f, 0.0f ,zOffset)));

       // surfaceNode->setTransform(glm::mat4());
        m_numSurfacesMapped ++;

        this->defaultSeat()->setPointerFocus(surfaceNode->surface(), glm::vec2());

    }else if(type == WaylandSurface::SurfaceType::POPUP ||
             type == WaylandSurface::SurfaceType::TRANSIENT){

        WaylandSurfaceNode *parentSurfaceNode;
        //if(surface->parentSurface() != NULL){
        //   parentSurfaceNode = this->getSurfaceNode(surface->parentSurface());
        //    glm::vec2 localPos = glm::vec2(surface->position());
        if(defaultSeat()->pointerFocus() != NULL){
           parentSurfaceNode = this->getSurfaceNode(defaultSeat()->pointerFocus());
           glm::vec2 localPos;
           if(type == WaylandSurface::SurfaceType::POPUP){
              localPos = this->defaultSeat()->pointer()->localPositon();
           }else{
              localPos = surface->position();
           }

           glm::vec3 position = glm::vec3(parentSurfaceNode->surfaceTransform() *
                                          glm::vec4((localPos + glm::vec2(surface->size()) / 2.0f) /
                                                       glm::vec2(parentSurfaceNode->surface()->size()), popupZOffset, 1));
           std::cout << "creating popup/transient window with parent " << parentSurfaceNode << " at position:" << std::endl;
           motorcar::Geometry::printVector(position);
           surfaceNode->setTransform(glm::translate(glm::mat4(), position));
           surfaceNode->setParentNode(parentSurfaceNode);
        }else{
            std::cout << "WARNING: creating popup/transient window with no parent " << std::endl;
             surfaceNode->setParentNode(this->scene());
             surfaceNode->setTransform(glm::mat4());
        }

        if(type == WaylandSurface::SurfaceType::POPUP ){
            this->defaultSeat()->setPointerFocus(surfaceNode->surface(), glm::vec2());
        }
    }else{

    }

    //WaylandSurfaceNode *surfaceNode = new WaylandSurfaceNode(surface, parentNode, transform);

    //this creates a new surface if needed but otherwise gives us the associated surface


    std::cout << "mapped surfaceNode " << surfaceNode << std::endl;




    surfaceNode->setMapped(true);
    //ensureKeyboardFocusIsValid(surface);
    return surfaceNode;
}

void WindowManager::unmapSurface(WaylandSurface *surface)
{
    WaylandSurfaceNode *surfaceNode = this->getSurfaceNode(surface);
    if(surfaceNode != NULL){
        surfaceNode->setMapped(false);
        ensureKeyboardFocusIsValid(surface);
    }else{
        std::cout << "Warning: surface unmapped but doesnt have associated surfaceNode" <<std::endl;
    }
}

void WindowManager::sendEvent(const Event &event)
{
    WaylandSurface *focus;
    switch(event.type()){
    case Event::EventType::MOUSE:
        focus = event.seat()->pointerFocus();
        if(focus != NULL){
            focus->sendEvent(event);
        }
        break;
    case Event::EventType::KEYBOARD:
        focus = event.seat()->keyboardFocus();
        if(focus != NULL){
            focus->sendEvent(event);
        }
        break;
    default:
        break;
    }
}

WaylandSurfaceNode *WindowManager::getSurfaceNode(WaylandSurface *surface) const
{
    if(surface != NULL && m_surfaceMap.count(surface)){
        return m_surfaceMap.find(surface)->second;
    }else{
        return NULL;
    }

}

void WindowManager::ensureKeyboardFocusIsValid(WaylandSurface *oldSurface)
{
    WaylandSurface *nextSurface = NULL;
//    if(!m_surfaceMap.empty()){
//        nextSurface = m_surfaceMap.begin()->first;
//    }
    std::map<WaylandSurface *, WaylandSurfaceNode *>::iterator it;
    for(it = m_surfaceMap.begin(); it != m_surfaceMap.end(); it++) {
        WaylandSurface * surface = it->first;
        if(surface->type() == WaylandSurface::SurfaceType::TOPLEVEL){
            nextSurface = surface;
            break;
        }
    }

    m_defaultSeat->ensureKeyboardFocusIsValid(oldSurface, nextSurface);
}
-}
