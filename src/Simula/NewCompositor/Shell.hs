module Simula.NewCompositor.Shell where

import Data.IORef
import Foreign
import Foreign.C

import Simula.Wayland

import Simula.NewCompositor.SceneGraph

data Shell = Shell {
  _shellScene :: IORef Scene,
  _shellDisplay :: IORef (Ptr C'wl_display)
  }

newShell :: IORef Scene -> IO Shell
newShell = undefined
{-
Shell::Shell(Scene *scene)
    :m_scene(scene)
{
    m_display = scene->compositor()->wlDisplay();
    wl_global_create(m_display,
                     &motorcar_shell_interface,
                      motorcar_shell_interface.version,
                     this,
                     Shell::bind_func);
}
-}

bindFunc :: Ptr C'wl_client -> Ptr () -> CUInt -> CUInt -> IO ()
bindFunc = undefined
{-
    struct wl_resource *resource = wl_resource_create(client, &motorcar_shell_interface, version, id);
    wl_resource_set_implementation(resource, &motorcarShellInterface, data, 0);
-}



{-
void get_motorcar_surface(struct wl_client *client,
                           struct wl_resource *resource,
                           uint32_t id,
                           struct wl_resource *surface_resource,
                          uint32_t clipping_mode,
                          uint32_t enable_depth_compositing)
{
    Shell *shell = static_cast<Shell*>(resource->data);

    WaylandSurface *surface = shell->scene()->compositor()->getSurfaceFromResource(surface_resource);

    WaylandSurface::ClippingMode clippingMode;

    switch(clipping_mode){
    case(MOTORCAR_SURFACE_CLIPPING_MODE_CUBOID):
        clippingMode = WaylandSurface::ClippingMode::CUBOID;
        break;
    case(MOTORCAR_SURFACE_CLIPPING_MODE_PORTAL):
        clippingMode = WaylandSurface::ClippingMode::PORTAL;
        break;
    default:
        clippingMode = WaylandSurface::ClippingMode::CUBOID;
        break;
    }

    //WaylandSurfaceNode * surfaceNode = shell->scene()->windowManager()->mapSurface(surface, type);

    surface->setClippingMode(clippingMode);
    surface->setIsMotorcarSurface(true);
    surface->setDepthCompositingEnabled(enable_depth_compositing!=0);

    std::cout << "depth compositing enabled = " << surface->depthCompositingEnabled() << std::endl;


    MotorcarSurfaceNode *mcsn = static_cast<MotorcarSurfaceNode *> (shell->scene()->windowManager()->createSurface(surface));

    mcsn->configureResource(client, id);


}


const static struct motorcar_shell_interface motorcarShellInterface = {
    get_motorcar_surface
};

Shell::Shell(Scene *scene)
    :m_scene(scene)
{
    m_display = scene->compositor()->wlDisplay();

    struct wl_global *global =0;

    global = wl_global_create(m_display,
                     &motorcar_shell_interface,
                      motorcar_shell_interface.version,
                     this,
                     Shell::bind_func);

    std::cout << "creating global shell object" <<std::endl;
    //struct motorcar_shell_interface *shell =
}
-}
