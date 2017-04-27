module Simula.Weston where

import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp

{#import Simula.WaylandServer#}

import Foreign
import Foreign.C

-- needs pkgconfig for libweston
#include "compositor.h"
#include "compositor-wayland.h"

{#pointer *weston_compositor as WestonCompositor newtype#}
{#pointer *weston_seat as WestonSeat newtype#}
{#pointer *weston_output as WestonOutput newtype#}
{#pointer *weston_keyboard as WestonKeyboard newtype#}
{#pointer *weston_pointer as WestonPointer newtype#}

instance WlListElement WestonSeat where
  linkOffset _ = {#offsetof weston_seat->link#}

instance WlListElement WestonOutput where
  linkOffset _ = {#offsetof weston_output->link#}

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
   ptrs <- wlListAll list
   return (WestonOutput <$> ptrs)

westonCompositorSeats :: WestonCompositor -> IO [WestonSeat]
westonCompositorSeats wc = do
   list <- WlList <$> {#get weston_compositor->seat_list#} wc
   ptrs <- wlListAll list
   return (WestonSeat <$> ptrs)
  
{#fun weston_seat_get_keyboard {`WestonSeat'} -> `WestonKeyboard' #}
{#fun weston_seat_get_pointer {`WestonSeat'} -> `WestonPointer' #}
