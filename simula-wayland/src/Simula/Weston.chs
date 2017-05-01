module Simula.Weston where

import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp

import Control.Monad
import Data.Proxy
import System.Posix.DynamicLinker
import Foreign
import Foreign.C
import Linear

{#context lib="libweston-1"#}
{#import Simula.WaylandServer#}

-- needs pkgconfig for libweston
#include "compositor.h"
#include "compositor-wayland.h"
#include "compositor-x11.h"
#include "util.h"
#include "GL/gl.h"
#include "EGL/egl.h"
#include "EGL/eglext.h"
#include "windowed-output-api.h"

{#pointer *weston_compositor as WestonCompositor newtype#}
deriving instance Eq WestonCompositor

{#pointer *weston_seat as WestonSeat newtype#}
deriving instance Eq WestonSeat

{#pointer *weston_output as WestonOutput newtype#}
deriving instance Eq WestonOutput

{#pointer *weston_keyboard as WestonKeyboard newtype#}
deriving instance Eq WestonKeyboard

{#pointer *weston_pointer as WestonPointer newtype#}
deriving instance Eq WestonPointer

{#pointer *weston_surface as WestonSurface newtype#}
deriving instance Eq WestonSurface
deriving instance Ord WestonSurface

{#pointer *weston_view as WestonView newtype#}
deriving instance Eq WestonView

instance WlListElement WestonCompositor WestonSeat where
  linkOffset _ _ = {#offsetof weston_seat->link#}

instance WlListElement WestonCompositor WestonOutput where
  linkOffset _ _ = {#offsetof weston_output->link#}

{#enum weston_compositor_backend as WestonCompositorBackend {underscoreToCase} #}

{#fun weston_compositor_create {`WlDisplay', `Ptr ()'} -> `WestonCompositor'#}

data WestonBackendConfig = WestonBackendConfig { configStructVersion :: CUInt,
                                                 configStructSize :: Int }
  deriving (Show, Eq, Ord)

{#pointer *weston_backend_config as WestonBackendConfigPtr -> WestonBackendConfig #}

instance Storable WestonBackendConfig where
  sizeOf _ = {#sizeof weston_backend_config#}
  alignment _ = {#alignof weston_backend_config#}
  peek ptr = WestonBackendConfig
             <$> {#get weston_backend_config->struct_version#} ptr
             <*> (fromIntegral <$> {#get weston_backend_config->struct_size#} ptr)
  poke ptr (WestonBackendConfig ver size) = do
    {#set weston_backend_config->struct_version#} ptr ver
    {#set weston_backend_config->struct_size#} ptr (fromIntegral size)


{#pointer *weston_x11_backend_config as WestonX11BackendConfigPtr -> WestonX11BackendConfig #}

data WestonX11BackendConfig = WestonX11BackendConfig {
  x11BaseConfig :: WestonBackendConfig,
  x11Fullscreen :: Bool,
  x11NoInput :: Bool,
  x11UsePixman :: Bool
  } deriving (Show, Eq, Ord)

instance Storable WestonX11BackendConfig where
  sizeOf _ = {#sizeof weston_x11_backend_config#}
  alignment _ = {#alignof weston_x11_backend_config#}
  peek ptr = WestonX11BackendConfig
             <$> peek (plusPtr (castPtr ptr) {#offsetof weston_x11_backend_config->base#})
             <*> {#get weston_x11_backend_config->fullscreen#} ptr
             <*> {#get weston_x11_backend_config->no_input#} ptr
             <*> {#get weston_x11_backend_config->use_pixman#} ptr
  poke ptr WestonX11BackendConfig{..} = do
    poke (plusPtr (castPtr ptr) {#offsetof weston_x11_backend_config->base#}) x11BaseConfig
    {#set weston_x11_backend_config->fullscreen#} ptr x11Fullscreen
    {#set weston_x11_backend_config->no_input#} ptr x11NoInput
    {#set weston_x11_backend_config->use_pixman#} ptr x11UsePixman


westonWaylandBackendConfigVersion :: CUInt
westonWaylandBackendConfigVersion = {#const WESTON_WAYLAND_BACKEND_CONFIG_VERSION#}

westonX11BackendConfigVersion :: CUInt
westonX11BackendConfigVersion = {#const WESTON_X11_BACKEND_CONFIG_VERSION#}

{#fun weston_compositor_load_backend {`WestonCompositor', `WestonCompositorBackend', `WestonBackendConfigPtr'} -> `Int' #}

westonCompositorOutputs :: WestonCompositor -> IO [WestonOutput]
westonCompositorOutputs wc = do
   list <- WlList <$> {#get weston_compositor->output_list#} wc
   ptrs <- wlListAll (Proxy :: Proxy WestonCompositor) list
   return (WestonOutput <$> ptrs)

westonCompositorSeats :: WestonCompositor -> IO [WestonSeat]
westonCompositorSeats wc = do
   list <- WlList <$> {#get weston_compositor->seat_list#} wc
   ptrs <- wlListAll (Proxy :: Proxy WestonCompositor) list
   return (WestonSeat <$> ptrs)

westonCompositorCreateSurfaceSignal :: WestonCompositor -> WlSignal
westonCompositorCreateSurfaceSignal (WestonCompositor ptr) = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_compositor->create_surface_signal#}

westonCompositorOutputPendingSignal :: WestonCompositor -> WlSignal
westonCompositorOutputPendingSignal (WestonCompositor ptr) = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_compositor->output_pending_signal#}

{#fun weston_seat_get_keyboard {`WestonSeat'} -> `WestonKeyboard' #}
{#fun weston_seat_get_pointer {`WestonSeat'} -> `WestonPointer' #}

instance WlListElement WestonSurface WestonView where
  linkOffset _ _ = {#offsetof weston_view->surface_link#}

westonSurfaceViews :: WestonSurface -> IO [WestonView]
westonSurfaceViews ws = do
   list <- WlList <$> {#get weston_surface->views#} ws
   ptrs <- wlListAll (Proxy :: Proxy WestonSurface) list
   return (WestonView <$> ptrs)

westonSurfaceWidth, westonSurfaceHeight :: WestonSurface -> IO Int
westonSurfaceWidth = {#get weston_surface->width#} >=> (return . fromIntegral)
westonSurfaceHeight = {#get weston_surface->height#} >=> (return . fromIntegral)

setWestonSurfaceWidth, setWestonSurfaceHeight :: WestonSurface -> Int -> IO ()
setWestonSurfaceWidth ptr = {#set weston_surface->width#} ptr . fromIntegral
setWestonSurfaceHeight ptr = {#set weston_surface->height#} ptr . fromIntegral

westonSurfaceSize :: WestonSurface -> IO (V2 Int)
westonSurfaceSize ws = V2 <$> westonSurfaceWidth ws <*> westonSurfaceHeight ws

westonViewPosX, westonViewPosY :: WestonView -> IO Float
westonViewPosX = {#get weston_view->geometry.x#} >=> (return . realToFrac)
westonViewPosY = {#get weston_view->geometry.y#} >=> (return . realToFrac)

westonViewPos :: WestonView -> IO (V2 Float)
westonViewPos view = V2 <$> westonViewPosX view <*> westonViewPosY view

{#pointer *weston_pointer_motion_event as WestonPointerMotionEvent newtype#}
{#pointer *weston_pointer_axis_event as WestonPointerAxisEvent newtype#}
{#enum wl_keyboard_key_state as WlKeyboardKeyState {underscoreToCase} #}

{#fun weston_pointer_send_motion {`WestonPointer', `CUInt', `WestonPointerMotionEvent'} -> `()'#}
{#fun weston_pointer_send_button {`WestonPointer', `CUInt', `CUInt', `CUInt'} -> `()'#}
{#fun weston_pointer_send_axis {`WestonPointer', `CUInt', `WestonPointerAxisEvent'} -> `()'#}
{#fun weston_pointer_set_focus {`WestonPointer', `WestonView', `Int', `Int'} ->`()'#}

{#fun weston_keyboard_send_key {`WestonKeyboard', `CUInt', `CUInt', `WlKeyboardKeyState'} -> `()'#}
{#fun weston_keyboard_set_focus {`WestonKeyboard', `WestonSurface'} -> `()'#}

{#fun weston_seat_set_keyboard_focus {`WestonSeat', `WestonSurface'} -> `()'#}

{#pointer *weston_buffer as WestonBuffer newtype#}
{#pointer *weston_buffer_reference as WestonBufferReference newtype#}
{#pointer *weston_subsurface as WestonSubsurface newtype#}

westonSurfaceBuffer :: WestonSurface -> WestonBuffer
westonSurfaceBuffer (WestonSurface ptr) = WestonBuffer $ plusPtr (castPtr ptr) {#offsetof weston_surface->buffer_ref.buffer#}

westonBufferResource :: WestonBuffer -> WlResource
westonBufferResource (WestonBuffer ptr) = WlResource $ plusPtr (castPtr ptr) {#offsetof weston_buffer->resource#}

instance WlListElement WestonSurface WestonSubsurface where
  linkOffset _ _ = {#offsetof weston_subsurface->parent_link#}

westonSurfaceSubsurfaces :: WestonSurface -> IO [WestonSubsurface]
westonSurfaceSubsurfaces ws = do
   list <- WlList <$> {#get weston_surface->subsurface_list#} ws
   ptrs <- wlListAll (Proxy :: Proxy WestonSurface) list
   return (WestonSubsurface <$> ptrs)

westonSubsurfaceSurface :: WestonSubsurface -> WestonSurface
westonSubsurfaceSurface (WestonSubsurface ptr) = WestonSurface $ plusPtr (castPtr ptr) {#offsetof weston_subsurface->surface#}

westonSurfaceIsYInverted :: WestonSurface -> IO Bool
westonSurfaceIsYInverted = {#get weston_buffer->y_inverted#} . westonSurfaceBuffer >=> (return . toBool)

{#fun weston_compositor_wake {`WestonCompositor'} -> `()'#}

{#fun setup_weston_log_handler {} -> `()'#}

{#pointer *xkb_rule_names as XkbRuleNames newtype#}
{#fun weston_compositor_set_xkb_rule_names {`WestonCompositor', `XkbRuleNames'} -> `()'#}

westonCompositorSetEmptyRuleNames :: WestonCompositor -> IO ()
westonCompositorSetEmptyRuleNames comp = allocaBytes {#sizeof xkb_rule_names#} $ weston_compositor_set_xkb_rule_names comp . XkbRuleNames . castPtr


westonSurfaceDestroySignal :: WestonSurface -> WlSignal
westonSurfaceDestroySignal (WestonSurface ptr) = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_surface->destroy_signal#}

westonSurfaceCommitSignal :: WestonSurface -> WlSignal
westonSurfaceCommitSignal (WestonSurface ptr) = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_surface->commit_signal#}

{#pointer EGLDisplay newtype#}
{#pointer EGLSurface newtype#}

{#pointer *weston_windowed_output_api as WestonWindowedOutputApi newtype#}
{#fun weston_windowed_output_get_api {`WestonCompositor'} -> `WestonWindowedOutputApi' #}

type WindowedOutputCreateFunc = WestonCompositor -> CString -> IO CInt
type WindowedOutputSetSizeFunc = WestonOutput -> CInt -> CInt -> IO CInt
foreign import ccall "dynamic" fromWindowedOutputCreateFuncPtr :: FunPtr WindowedOutputCreateFunc -> WindowedOutputCreateFunc
foreign import ccall "dynamic" fromWindowedOutputSetSizeFuncPtr :: FunPtr WindowedOutputSetSizeFunc -> WindowedOutputSetSizeFunc

westonWindowedOutputCreate :: WestonWindowedOutputApi -> WestonCompositor -> String -> IO Bool
westonWindowedOutputCreate api comp str = do
  ptr <- {#get weston_windowed_output_api->output_create #} api 
  withCString str $ \strPtr -> toBool <$> fromWindowedOutputCreateFuncPtr ptr comp strPtr  

westonWindowedOutputSetSize :: WestonWindowedOutputApi -> WestonOutput -> CInt -> CInt -> IO Bool
westonWindowedOutputSetSize api outp w h = do
  ptr <- {#get weston_windowed_output_api->output_set_size #} api 
  toBool <$> fromWindowedOutputSetSizeFuncPtr ptr outp w h

{#fun weston_output_enable {`WestonOutput'} -> `Int'#}
{#fun weston_output_set_scale {`WestonOutput', `Int'} -> `()'#}
{#fun weston_output_set_transform {`WestonOutput', `Int'} -> `()'#}


{#pointer EGLContext newtype#}
{#fun eglGetCurrentContext {} -> `EGLContext'#}
{#fun eglMakeCurrent {`EGLDisplay', `EGLSurface', `EGLSurface', `EGLContext'} -> `()'#}


{-
	void render_manager::paint(pixman_region32_t *damage)
{
    if (dirty_context)
        load_context();

    // This is a hack, weston renderer_state is a struct and the EGLSurface is the first field
    // In the future this might change so we need to track changes in weston
    EGLSurface surf = *(EGLSurface*)output->handle->renderer_state;
    weston_gl_renderer *gr = (weston_gl_renderer*) core->ec->renderer;
    eglMakeCurrent(gr->display, surf, surf, gr->context);

    GL_CALL(glViewport(output->handle->x, output->handle->y,
                output->handle->width, output->handle->height));

    if (renderer) {
        OpenGL::bind_context(ctx);
        renderer();

        wl_signal_emit(&output->handle->frame_signal, output->handle);
        eglSwapBuffers(gr->display, surf);
    } else {
        core->weston_repaint(output->handle, damage);
    }

    if (constant_redraw) {
        wl_event_loop_add_idle(wl_display_get_event_loop(core->ec->wl_display),
                redraw_idle_cb, output);
    }
}

void render_manager::pre_paint()
{
    std::vector<effect_hook_t*> active_effects;
    for (auto effect : output_effects) {
        active_effects.push_back(effect);
    }

    for (auto& effect : active_effects)
        (*effect)();
}

void repaint_output_callback(weston_output *o, pixman_region32_t *damage)
{
    auto output = core->get_output(o);
    if (output) {
        output->render->pre_paint();
        output->render->paint(damage);
    }
}

void wayfire_core::hijack_renderer()
{
    weston_renderer_repaint = core->ec->renderer->repaint_output;
    core->ec->renderer->repaint_output = repaint_output_callback;
}

	struct weston_gl_surface_state {
    GLfloat color[4];
    void *shader;
    GLuint textures[3];
};


void render_surface(weston_surface *surface, int x, int y, glm::mat4 transform, glm::vec4 color)
{
    if (!surface->is_mapped || !surface->renderer_state)
        return;

    auto gs = (weston_gl_surface_state *) surface->renderer_state;

    wayfire_geometry geometry;
    geometry.origin = {x, y};
    geometry.size = {surface->width, surface->height};

    for (int i = 0; i < 3 && gs->textures[i]; i++) {
        OpenGL::render_transformed_texture(gs->textures[i], geometry, transform,
                                           color, TEXTURE_TRANSFORM_USE_COLOR);
    }

    weston_subsurface *sub;
    if (!wl_list_empty(&surface->subsurface_list)) {
        wl_list_for_each(sub, &surface->subsurface_list, parent_link) {
            if (sub && sub->surface != surface)
                render_surface(sub->surface, sub->position.x + x, sub->position.y + y, transform, color);
        }
    }
}

	
    void render_transformed_texture(GLuint tex, const wayfire_geometry& g, glm::mat4 model, glm::vec4 color, uint32_t bits) {
        GL_CALL(glUseProgram(bound->program));

        GL_CALL(glUniformMatrix4fv(bound->mvpID, 1, GL_FALSE, &model[0][0]));
        GL_CALL(glUniform4fv(bound->colorID, 1, &color[0]));

        GL_CALL(glEnable(GL_BLEND));
        GL_CALL(glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA));
        render_texture(tex, g, bits | DONT_RELOAD_PROGRAM);
        GL_CALL(glDisable(GL_BLEND));

-}
