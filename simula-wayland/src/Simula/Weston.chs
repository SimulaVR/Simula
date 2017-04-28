module Simula.Weston where

import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp

import Control.Monad
import Data.Proxy
import Foreign
import Foreign.C
import Linear

{#context lib="libweston-1"#}
{#import Simula.WaylandServer#}

-- needs pkgconfig for libweston
#include "compositor.h"
#include "compositor-wayland.h"
#include "util.h"

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
                                                 configStructSize :: CSize }
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

westonWaylandBackendConfigVersion :: CUInt
westonWaylandBackendConfigVersion = {#const WESTON_WAYLAND_BACKEND_CONFIG_VERSION#}

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

