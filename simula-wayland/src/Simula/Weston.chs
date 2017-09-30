{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simula.Weston where

import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp


import Control.Monad
import Data.Proxy
import Data.Word
import System.Posix.DynamicLinker
import System.Posix.Types
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
#include "xwayland-api.h"

{#pointer *weston_compositor as WestonCompositor newtype#}
deriving instance Eq WestonCompositor
deriving instance Storable WestonCompositor

{#pointer *weston_seat as WestonSeat newtype#}
deriving instance Eq WestonSeat
deriving instance Storable WestonSeat

{#pointer *weston_output as WestonOutput newtype#}
deriving instance Eq WestonOutput
deriving instance Storable WestonOutput

{#pointer *weston_keyboard as WestonKeyboard newtype#}
deriving instance Eq WestonKeyboard

{#pointer *weston_pointer as WestonPointer newtype#}
deriving instance Eq WestonPointer
deriving instance Storable WestonPointer

{#pointer *weston_surface as WestonSurface newtype#}
deriving instance Eq WestonSurface
deriving instance Ord WestonSurface
deriving instance Storable WestonSurface

{#pointer *weston_view as WestonView newtype#}
deriving instance Eq WestonView

instance WlListElement WestonCompositor WestonSeat where
  linkOffset _ _ = {#offsetof weston_seat->link#}

instance WlListElement WestonCompositor WestonOutput where
  linkOffset _ _ = {#offsetof weston_output->link#}

instance WlListElement WestonLayer WestonView where
  linkOffset _ _ = {#offsetof weston_view->layer_link#}

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

westonCompositorOutputCreatedSignal :: WestonCompositor -> WlSignal
westonCompositorOutputCreatedSignal (WestonCompositor ptr) = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_compositor->output_created_signal#}

{#fun weston_seat_get_keyboard {`WestonSeat'} -> `WestonKeyboard' #}
{#fun weston_seat_get_pointer {`WestonSeat'} -> `WestonPointer' #}

instance WlListElement WestonSurface WestonView where
  linkOffset _ _ = {#offsetof weston_view->surface_link#}

westonSurfaceViews :: WestonSurface -> IO [WestonView]
westonSurfaceViews ws = do
   list <- WlList <$> {#get weston_surface->views#} ws
   ptrs <- wlListAll (Proxy :: Proxy WestonSurface) list
   return (WestonView <$> ptrs)

westonViewSurface :: WestonView -> IO WestonSurface
westonViewSurface = {#get weston_view->surface#}

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

{#enum wl_keyboard_key_state as WlKeyboardKeyState {underscoreToCase} #}

{#fun weston_keyboard_send_key {`WestonKeyboard', `CUInt', `CUInt', `WlKeyboardKeyState'} -> `()'#}
{#fun weston_keyboard_set_focus {`WestonKeyboard', `WestonSurface'} -> `()'#}

{#fun weston_seat_set_keyboard_focus {`WestonSeat', `WestonSurface'} -> `()'#}

{#pointer *weston_buffer as WestonBuffer newtype#}
{#pointer *weston_buffer_reference as WestonBufferReference newtype#}
{#pointer *weston_subsurface as WestonSubsurface newtype#}

westonSurfaceBuffer :: WestonSurface -> IO WestonBuffer
westonSurfaceBuffer ws = do
  buffer_ref <- {#get weston_surface->buffer_ref#} ws
  return $ WestonBuffer $ plusPtr (castPtr buffer_ref) {#offsetof weston_buffer_reference->buffer#}

-- bug with c2hs?
westonBufferResource :: WestonBuffer -> IO WlResource
westonBufferResource (WestonBuffer ptr) = do
  resourcePtr <- peekByteOff ptr 0 
  return $ WlResource resourcePtr

{#fun weston_buffer_get_shm_buffer {`WestonBuffer'} -> `WlShmBuffer'#}
{#fun weston_buffer_get_legacy_buffer {`WestonBuffer'} -> `Ptr ()'#}

westonBufferWidth :: WestonBuffer -> IO CInt
westonBufferWidth = {#get weston_buffer->width#} 
westonBufferHeight :: WestonBuffer -> IO CInt
westonBufferHeight = {#get weston_buffer->height#}

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
westonSurfaceIsYInverted surf =  westonSurfaceBuffer surf >>= ({#get weston_buffer->y_inverted#} >=> (return . toBool))

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
deriving instance Eq EGLDisplay
{#pointer EGLSurface newtype#}
deriving instance Eq EGLSurface

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
deriving instance Eq EGLContext
{#fun eglMakeCurrent {`EGLDisplay', `EGLSurface', `EGLSurface', `EGLContext'} -> `()'#}

{#pointer *weston_gl_output_state as WestonGlOutputState newtype#}
{#pointer *weston_gl_renderer as WestonGlRenderer newtype#}

westonOutputRendererSurface :: WestonOutput -> IO EGLSurface
westonOutputRendererSurface output = do
  ptr <- castPtr <$> {#get weston_output->renderer_state#} output
  surfPtr <- {#get weston_gl_output_state->egl_surface#} $ WestonGlOutputState ptr
  return $ EGLSurface (castPtr surfPtr)

westonCompositorGlRenderer :: WestonCompositor -> IO WestonGlRenderer
westonCompositorGlRenderer wc = do
  ptr <- castPtr <$> {#get weston_compositor->renderer#} wc
  return $ WestonGlRenderer ptr

westonGlRendererContext :: WestonGlRenderer -> IO EGLContext
westonGlRendererContext rend = do
  ptr <- {#get weston_gl_renderer->egl_context#} rend
  return $ EGLContext (castPtr ptr)

westonGlRendererDisplay :: WestonGlRenderer -> IO EGLDisplay
westonGlRendererDisplay rend = do
  ptr <- {#get weston_gl_renderer->egl_display#} rend
  return $ EGLDisplay (castPtr ptr)

{#pointer *pixman_region32_t as PixmanRegion32 newtype#}

type RepaintOutputFunc = WestonOutput -> PixmanRegion32 -> IO ()
foreign import ccall "wrapper" createRendererRepaintOutputFunc :: RepaintOutputFunc -> IO (FunPtr RepaintOutputFunc)
foreign import ccall "dynamic" fromRendererRepaintOutputFunc :: FunPtr RepaintOutputFunc -> RepaintOutputFunc

{#pointer *weston_layer as WestonLayer newtype#}
deriving instance Eq WestonLayer
{#enum weston_layer_position as WestonLayerPosition {underscoreToCase} #}
{#fun weston_layer_init {`WestonLayer', `WestonCompositor'} -> `()' #}

newWestonLayer :: WestonCompositor -> IO WestonLayer
newWestonLayer wc = do
  ptr <- WestonLayer <$> mallocBytes {#sizeof weston_layer#}
  weston_layer_init ptr wc
  return ptr

westonLayerViews :: WestonLayer -> IO [WestonView]
westonLayerViews (WestonLayer wlPtr) = do
  let list = WlList $ plusPtr (castPtr wlPtr) {#offsetof weston_layer->view_list#}
  ptrs <- wlListAll (Proxy :: Proxy WestonLayer) list
  return (WestonView <$> ptrs)

westonCompositorCursorLayer :: WestonCompositor -> WestonLayer
westonCompositorCursorLayer (WestonCompositor ptr) = WestonLayer $ plusPtr (castPtr ptr) {#offsetof weston_compositor->cursor_layer#}
  
  

{#fun weston_layer_set_position {`WestonLayer', `WestonLayerPosition'} -> `()' #}

{#fun weston_surface_create {`WestonCompositor'} -> `WestonSurface' #}
{#fun weston_view_create {`WestonSurface'} -> `WestonView' #}

{#fun weston_surface_set_color {`WestonSurface', `Float', `Float', `Float', `Float'} -> `()'#}
{#fun weston_surface_set_size {`WestonSurface', `Int', `Int'} -> `()'#}
{#fun weston_view_set_position {`WestonView', `Float', `Float'} -> `()'#}
{#fun weston_view_update_transform {`WestonView'} -> `()'#}

{#fun pixman_region32_fini {`PixmanRegion32'} -> `()' #}
{#fun pixman_region32_init_rect {`PixmanRegion32', `Int', `Int', `Int', `Int'} -> `()' #}

{#pointer *weston_layer_entry as WestonLayerEntry newtype#}
{#fun weston_layer_entry_insert {`WestonLayerEntry', `WestonLayerEntry'} -> `()'#}

westonSurfaceOpaque :: WestonSurface -> PixmanRegion32
westonSurfaceOpaque (WestonSurface ptr) = PixmanRegion32 $ plusPtr (castPtr ptr) {#offsetof weston_surface->opaque#}

westonSurfaceInput :: WestonSurface -> PixmanRegion32
westonSurfaceInput (WestonSurface ptr) = PixmanRegion32 $ plusPtr (castPtr ptr) {#offsetof weston_surface->opaque#}

westonLayerViewList :: WestonLayer -> WestonLayerEntry
westonLayerViewList (WestonLayer ptr) = WestonLayerEntry $ plusPtr (castPtr ptr) {#offsetof weston_layer->view_list#}

westonViewLayerEntry :: WestonView -> WestonLayerEntry
westonViewLayerEntry (WestonView ptr) = WestonLayerEntry $ plusPtr (castPtr ptr) {#offsetof weston_view->layer_link#}


getRepaintOutput :: WestonCompositor -> IO RepaintOutputFunc
getRepaintOutput wc = fromRendererRepaintOutputFunc <$> {#get weston_compositor->renderer->repaint_output#} wc

setRepaintOutput :: WestonCompositor -> FunPtr RepaintOutputFunc -> IO ()
setRepaintOutput wc new = {#set weston_compositor->renderer->repaint_output#} wc new


emitOutputFrameSignal :: WestonOutput -> IO ()
emitOutputFrameSignal (WestonOutput ptr) = let signal = WlSignal $ plusPtr (castPtr ptr) {#offsetof weston_output->frame_signal#}
                                           in wl_signal_emit signal $ castPtr ptr
  

{#fun eglSwapBuffers {`EGLDisplay', `EGLSurface'} -> `()'#}
{#fun eglGetError {} -> `Int'#}
{#fun eglInitialize {`EGLDisplay', id `Ptr CInt', id `Ptr CInt'} -> `()'#}

type SurfaceGetContentSizeFunc = WestonSurface -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "dynamic" fromSurfaceGetContentSizeFunc :: FunPtr SurfaceGetContentSizeFunc -> SurfaceGetContentSizeFunc

{#pointer *weston_gl_surface_state as WestonGlSurfaceState newtype#}
westonSurfaceGlState :: WestonSurface -> IO (Maybe WestonGlSurfaceState)
westonSurfaceGlState surf = do
  ptr <- {#get weston_surface->renderer_state#} surf
  if ptr == nullPtr then return Nothing else return $ Just (WestonGlSurfaceState (castPtr ptr))

westonGlStateNumTextures :: WestonGlSurfaceState -> IO CInt
westonGlStateNumTextures = {#get weston_gl_surface_state->num_textures#}

westonGlStateTextureIds :: WestonGlSurfaceState -> IO [Word32]
westonGlStateTextureIds st@(WestonGlSurfaceState ptr) = do
  num <- westonGlStateNumTextures st
  peekArray (fromIntegral num) (plusPtr (castPtr ptr) {#offsetof weston_gl_surface_state->textures#})


{#fun weston_output_schedule_repaint {`WestonOutput'} -> `()'#}

westonSurfaceSetMapped :: WestonSurface -> Bool -> IO ()
westonSurfaceSetMapped = {#set weston_surface->is_mapped#}

westonSurfaceMapped :: WestonSurface -> IO Bool
westonSurfaceMapped = {#get weston_surface->is_mapped#}

westonViewSetMapped :: WestonView -> Bool -> IO ()
westonViewSetMapped = {#set weston_view->is_mapped#}

westonViewSetOutput :: WestonView -> WestonOutput -> IO ()
westonViewSetOutput = {#set weston_view->output#}

{#pointer *weston_pointer_grab as WestonPointerGrab newtype#}
{#pointer *weston_pointer_grab_interface  as WestonPointerGrabInterfacePtr -> WestonPointerGrabInterface#}
{#pointer *weston_pointer_motion_event as WestonPointerMotionEvent newtype#}
{#pointer *weston_pointer_axis_event as WestonPointerAxisEvent newtype#}

type PointerGrabUnaryFunc = WestonPointerGrab -> IO ()
type PointerGrabMotionFunc = WestonPointerGrab -> CUInt -> WestonPointerMotionEvent -> IO ()
type PointerGrabButtonFunc = WestonPointerGrab -> CUInt -> CUInt -> CUInt -> IO ()
type PointerGrabAxisFunc = WestonPointerGrab -> CUInt -> WestonPointerAxisEvent -> IO ()
type PointerGrabAxisSourceFunc = WestonPointerGrab -> CUInt -> IO ()

foreign import ccall "wrapper" createPointerGrabUnaryFunc :: PointerGrabUnaryFunc -> IO (FunPtr PointerGrabUnaryFunc)
foreign import ccall "wrapper" createPointerGrabMotionFunc :: PointerGrabMotionFunc -> IO (FunPtr PointerGrabMotionFunc)
foreign import ccall "wrapper" createPointerGrabButtonFunc :: PointerGrabButtonFunc -> IO (FunPtr PointerGrabButtonFunc)
foreign import ccall "wrapper" createPointerGrabAxisFunc :: PointerGrabAxisFunc -> IO (FunPtr PointerGrabAxisFunc)
foreign import ccall "wrapper" createPointerGrabAxisSourceFunc :: PointerGrabAxisSourceFunc -> IO (FunPtr PointerGrabAxisSourceFunc)

data WestonPointerGrabInterface = WestonPointerGrabInterface {
  grabPointerFocus :: WestonPointerGrab -> IO (),
  grabPointerMotion :: PointerGrabMotionFunc,
  grabPointerButton :: PointerGrabButtonFunc,
  grabPointerAxis :: PointerGrabAxisFunc,
  grabPointerAxisSource :: PointerGrabAxisSourceFunc,
  grabPointerFrame :: WestonPointerGrab -> IO (),
  grabPointerCancel :: WestonPointerGrab -> IO ()
  }

instance Storable WestonPointerGrabInterface where
  sizeOf _ = {#sizeof weston_pointer_grab_interface#}
  alignment _ = {#alignof weston_pointer_grab_interface#}
  peek = error "can't peek WestonPointerGrabInterface"
  poke ptr WestonPointerGrabInterface{..} = do
    createPointerGrabUnaryFunc grabPointerFocus >>= {#set weston_pointer_grab_interface->focus#} ptr
    createPointerGrabMotionFunc grabPointerMotion >>= {#set weston_pointer_grab_interface->motion#} ptr
    createPointerGrabButtonFunc grabPointerButton >>= {#set weston_pointer_grab_interface->button#} ptr
    createPointerGrabAxisFunc grabPointerAxis >>= {#set weston_pointer_grab_interface->axis#} ptr
    createPointerGrabAxisSourceFunc grabPointerAxisSource >>= {#set weston_pointer_grab_interface->axis_source#} ptr
    createPointerGrabUnaryFunc grabPointerFrame >>= {#set weston_pointer_grab_interface->frame#} ptr
    createPointerGrabUnaryFunc grabPointerCancel >>= {#set weston_pointer_grab_interface->cancel#} ptr


{#fun weston_pointer_set_focus {`WestonPointer',`WestonView', `Int', `Int'} -> `()'#}
{#fun weston_pointer_send_motion {`WestonPointer', `CUInt', `WestonPointerMotionEvent'} -> `()'#}
{#fun weston_pointer_send_button {`WestonPointer', `CUInt', `CUInt', `CUInt'} -> `()'#}
{#fun weston_pointer_send_axis {`WestonPointer', `CUInt', `WestonPointerAxisEvent'} -> `()'#}
{#fun weston_pointer_send_axis_source {`WestonPointer', `CUInt'} -> `()'#}
{#fun weston_pointer_send_frame {`WestonPointer'} -> `()'#}

westonPointerFromGrab :: WestonPointerGrab -> IO WestonPointer
westonPointerFromGrab = {#get weston_pointer_grab->pointer#}

defaultWestonPointerGrabInterface :: WestonPointerGrabInterface
defaultWestonPointerGrabInterface = WestonPointerGrabInterface {
  grabPointerFocus = error "Must implement focus",
  grabPointerMotion = \grab time event -> westonPointerFromGrab grab >>= \ptr ->
      weston_pointer_send_motion ptr time event,
  grabPointerButton = error "Must implement button for focus",
  grabPointerAxis = \grab time event -> westonPointerFromGrab grab >>= \ptr ->
      weston_pointer_send_axis ptr time event,
  grabPointerAxisSource = \grab source -> westonPointerFromGrab grab >>= \ptr ->
      weston_pointer_send_axis_source ptr source,
  grabPointerFrame = \grab -> westonPointerFromGrab grab >>= weston_pointer_send_frame,
  grabPointerCancel = \_ -> return ()
  }
   
{#fun weston_compositor_set_default_pointer_grab {`WestonCompositor', `WestonPointerGrabInterfacePtr'} -> `()' #}

westonPointerPosition :: WestonPointer -> IO (V2 Int)
westonPointerPosition wp = V2
                           <$> fmap fromIntegral ({#get weston_pointer->x#} wp)
                           <*> fmap fromIntegral ({#get weston_pointer->y#} wp)

westonPointerSeat :: WestonPointer -> IO WestonSeat
westonPointerSeat = {#get weston_pointer->seat#}


{#fun weston_surface_get_position_x {`WestonSurface'} -> `Int' #}
{#fun weston_surface_get_position_y {`WestonSurface'} -> `Int' #}
{#fun weston_surface_get_width {`WestonSurface'} -> `Int' #}
{#fun weston_surface_get_height {`WestonSurface'} -> `Int' #}


{#fun weston_compositor_load_xwayland {`WestonCompositor'} -> `Bool' #}


{#pointer *weston_xwayland as WestonXWayland newtype#}
{#pointer *weston_xwayland_api as WestonXWaylandApiPtr -> WestonXWaylandApi #}

data WestonXWaylandApi = WestonXWaylandApi {
  apiXWaylandGet :: XWaylandApiGetFunc,
  apiXWaylandListen :: XWaylandApiListenFunc,
  apiXWaylandXserverLoaded :: XWaylandApiXserverLoadedFunc,
  apiXWaylandXserverExited :: XWaylandApiXserverExitedFunc
}

instance Storable WestonXWaylandApi where
  sizeOf _ = {#sizeof weston_xwayland_api#}
  alignment _ = {#alignof weston_xwayland_api#}
  peek ptr = WestonXWaylandApi
             <$> (fromXWaylandApiGetFuncPtr <$> {#get weston_xwayland_api->get#} ptr)
             <*> (fromXWaylandApiListenFuncPtr <$> {#get weston_xwayland_api->listen#} ptr)
             <*> (fromXWaylandApiXserverLoadedFuncPtr <$> {#get weston_xwayland_api->xserver_loaded#} ptr)
             <*> (fromXWaylandApiXserverExitedFuncPtr <$> {#get weston_xwayland_api->xserver_exited#} ptr)

    
  poke = error "No poking"


type XWaylandSpawnXserverFunc = {#type weston_xwayland_spawn_xserver_func_t #}
type XWaylandApiGetFunc = WestonCompositor -> IO WestonXWayland
type XWaylandApiListenFunc = WestonXWayland -> Ptr () -> XWaylandSpawnXserverFunc -> IO CInt
type XWaylandApiXserverLoadedFunc = WestonXWayland -> WlClient -> CInt -> IO ()
type XWaylandApiXserverExitedFunc = WestonXWayland -> CInt -> IO ()

foreign import ccall "wrapper" createXWaylandSpawnXserverFunc :: XWaylandSpawnXserverFunc -> IO (FunPtr XWaylandSpawnXserverFunc)
foreign import ccall "dynamic" fromXWaylandApiGetFuncPtr :: FunPtr XWaylandApiGetFunc -> XWaylandApiGetFunc
foreign import ccall "dynamic" fromXWaylandApiListenFuncPtr :: FunPtr XWaylandApiListenFunc -> XWaylandApiListenFunc
foreign import ccall "dynamic" fromXWaylandApiXserverLoadedFuncPtr :: FunPtr XWaylandApiXserverLoadedFunc -> XWaylandApiXserverLoadedFunc
foreign import ccall "dynamic" fromXWaylandApiXserverExitedFuncPtr :: FunPtr XWaylandApiXserverExitedFunc -> XWaylandApiXserverExitedFunc

{#fun weston_xwayland_get_api {`WestonCompositor'} -> `WestonXWaylandApiPtr' #}

