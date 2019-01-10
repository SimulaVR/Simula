{-# LANGUAGE RecordWildCards #-}
module Simula.WestonDesktop where

{#context lib="libweston-desktop"#}

{#import Simula.WaylandServer#}
{#import Simula.Weston#}

import Data.Kind
import Foreign
import Foreign.C

#include "libweston-desktop.h"
#include "util.h"

{#enum weston_desktop_surface_edge as WestonDesktopSurfaceEdge {underscoreToCase} #}
{#pointer *weston_desktop_client as WestonDesktopClient newtype#}
{#pointer *weston_desktop_surface as WestonDesktopSurface newtype#}
deriving instance Eq WestonDesktopSurface
deriving instance Ord WestonDesktopSurface


type family AsC (a :: *) where
  AsC Bool = CInt
  AsC WestonDesktopSurfaceEdge = CInt
  AsC (a -> b) = AsC a -> AsC b
  AsC (Ptr a) = Ptr ()
  AsC a = a

class ForeignData (a :: *) where
  toHaskell :: AsC a -> a
  fromHaskell :: a -> AsC a

instance ForeignData Bool where
  toHaskell = toBool
  fromHaskell = fromBool

instance ForeignData WestonDesktopSurfaceEdge where
  toHaskell = toEnum . fromIntegral
  fromHaskell = fromIntegral . fromEnum

instance (ForeignData a, ForeignData b) => ForeignData (a -> b) where
  toHaskell f = toHaskell . f . fromHaskell
  fromHaskell f = fromHaskell . f . toHaskell

instance ForeignData (Ptr a) where
  toHaskell = castPtr
  fromHaskell = castPtr

instance {-# OVERLAPS #-} (a ~ AsC a) => ForeignData a where
  toHaskell = id
  fromHaskell = id

type WithUserData a = Ptr a -> IO ()
type PingTimeoutFunc a = WestonDesktopClient -> WithUserData a
type PongFunc a = WestonDesktopClient -> WithUserData a
type SurfaceAddedFunc a = WestonDesktopSurface -> WithUserData a
type SurfaceRemovedFunc a = WestonDesktopSurface -> WithUserData a
type CommittedFunc a =  WestonDesktopSurface -> CInt -> CInt -> WithUserData a
type ShowWindowMenuFunc a = WestonDesktopSurface -> WestonSeat -> CInt -> CInt -> WithUserData a
type SetParentFunc a = WestonDesktopSurface -> WestonDesktopSurface -> WithUserData a
type MoveFunc a = WestonDesktopSurface -> WestonSeat -> CUInt -> WithUserData a
type ResizeFunc a = WestonDesktopSurface -> WestonSeat -> CUInt -> WestonDesktopSurfaceEdge -> WithUserData a
type FullscreenRequestedFunc a = WestonDesktopSurface -> Bool -> WestonOutput -> WithUserData a
type MaximizedRequestedFunc a = WestonDesktopSurface -> Bool -> WithUserData a
type MinimizedRequestedFunc a = WestonDesktopSurface -> WithUserData a
type SetXwaylandPositionFunc a = WestonDesktopSurface -> CInt -> CInt -> WithUserData a


foreign import ccall "wrapper" createPingTimeoutFuncPtr :: AsC (PingTimeoutFunc a) -> IO (FunPtr (AsC (PingTimeoutFunc a)))
foreign import ccall "wrapper" createPongFuncPtr :: AsC (PongFunc a) -> IO (FunPtr (AsC (PongFunc a)))
foreign import ccall "wrapper" createSurfaceAddedFuncPtr :: AsC (SurfaceAddedFunc a) -> IO (FunPtr (AsC (SurfaceAddedFunc a)))
foreign import ccall "wrapper" createSurfaceRemovedFuncPtr :: AsC (SurfaceRemovedFunc a) -> IO (FunPtr (AsC (SurfaceRemovedFunc a)))
foreign import ccall "wrapper" createCommittedFuncPtr :: AsC (CommittedFunc a) -> IO (FunPtr (AsC (CommittedFunc a)))
foreign import ccall "wrapper" createShowWindowMenuFuncPtr :: AsC (ShowWindowMenuFunc a) -> IO (FunPtr (AsC (ShowWindowMenuFunc a)))
foreign import ccall "wrapper" createSetParentFuncPtr :: AsC (SetParentFunc a) -> IO (FunPtr (AsC (SetParentFunc a)))
foreign import ccall "wrapper" createMoveFuncPtr :: AsC (MoveFunc a) -> IO (FunPtr (AsC (MoveFunc a)))
foreign import ccall "wrapper" createResizeFuncPtr :: AsC (ResizeFunc a) -> IO (FunPtr (AsC (ResizeFunc a)))
foreign import ccall "wrapper" createFullscreenRequestedFuncPtr :: AsC (FullscreenRequestedFunc a) -> IO (FunPtr (AsC (FullscreenRequestedFunc a)))
foreign import ccall "wrapper" createMaximizedRequestedFuncPtr :: AsC (MaximizedRequestedFunc a) -> IO (FunPtr (AsC (MaximizedRequestedFunc a)))
foreign import ccall "wrapper" createMinimizedRequestedFuncPtr :: AsC (MinimizedRequestedFunc a) -> IO (FunPtr (AsC (MinimizedRequestedFunc a)))
foreign import ccall "wrapper" createSetXwaylandPositionFuncPtr :: AsC (SetXwaylandPositionFunc a) -> IO (FunPtr (AsC (SetXwaylandPositionFunc a)))


data WestonDesktopApi a = WestonDesktopApi {
  apiPingTimeout :: PingTimeoutFunc a,
  apiPong :: PongFunc a,
  apiSurfaceAdded :: SurfaceAddedFunc a,
  apiSurfaceRemoved :: SurfaceRemovedFunc a,
  apiCommitted :: CommittedFunc a,
  apiShowWindowMenu :: ShowWindowMenuFunc a,
  apiSetParent :: SetParentFunc a,
  apiMove :: MoveFunc a,
  apiResize :: ResizeFunc a,
  apiFullscreenRequested :: FullscreenRequestedFunc a,
  apiMaximizedRequested :: MaximizedRequestedFunc a,
  apiMinimizedRequested :: MinimizedRequestedFunc a,
  apiSetXwaylandPosition :: SetXwaylandPositionFunc a
  }

defaultWestonDesktopApi :: WestonDesktopApi a
defaultWestonDesktopApi = WestonDesktopApi {
  apiPingTimeout = \_ _ -> return (),
  apiPong = \_ _ -> return (),
  apiSurfaceAdded = \_ _ -> return (),
  apiSurfaceRemoved = \_ _ -> return (),
  apiCommitted = \_ _ _ _ -> return (),
  apiShowWindowMenu = \_ _ _ _ _ -> return (),
  apiSetParent = \_ _ _ -> return (),
  apiMove = \_ _ _ _ -> return (),
  apiResize = \_ _ _ _ _ -> return (),
  apiFullscreenRequested = \_ _ _ _ -> return (),
  apiMaximizedRequested = \_ _ _ -> return (),
  apiMinimizedRequested = \_ _ -> return (),
  apiSetXwaylandPosition = \_ _ _ _-> return ()
  }

{#pointer *weston_desktop_api as WestonDesktopApiPtr -> `WestonDesktopApi ()'#}

--TODO Template haskell this?
instance Storable (WestonDesktopApi a) where
  sizeOf _ = {#sizeof weston_desktop_api#}
  alignment _ = {#alignof weston_desktop_api#}
  peek = error "can't peek WestonDesktopApi"
  poke ptr WestonDesktopApi{..} = do
    {#set weston_desktop_api->struct_size#} ptr {#sizeof weston_desktop_api#}
    createPingTimeoutFuncPtr (fromHaskell apiPingTimeout) >>= {#set weston_desktop_api->ping_timeout#} ptr 
    createPongFuncPtr (fromHaskell apiPong) >>= {#set weston_desktop_api->pong#} ptr 
    createSurfaceAddedFuncPtr (fromHaskell apiSurfaceAdded) >>= {#set weston_desktop_api->surface_added#} ptr 
    createSurfaceRemovedFuncPtr (fromHaskell apiSurfaceRemoved) >>= {#set weston_desktop_api->surface_removed#} ptr 
    createCommittedFuncPtr (fromHaskell apiCommitted) >>= {#set weston_desktop_api->committed#} ptr 
    createShowWindowMenuFuncPtr (fromHaskell apiShowWindowMenu) >>= {#set weston_desktop_api->show_window_menu#} ptr 
    createSetParentFuncPtr (fromHaskell apiSetParent) >>= {#set weston_desktop_api->set_parent#} ptr 
    createMoveFuncPtr (fromHaskell apiMove) >>= {#set weston_desktop_api->move#} ptr 
    createResizeFuncPtr (fromHaskell apiResize) >>= {#set weston_desktop_api->resize#} ptr 
    createFullscreenRequestedFuncPtr (fromHaskell apiFullscreenRequested) >>= {#set weston_desktop_api->fullscreen_requested#} ptr 
    createMaximizedRequestedFuncPtr (fromHaskell apiMaximizedRequested) >>= {#set weston_desktop_api->maximized_requested#} ptr 
    createMinimizedRequestedFuncPtr (fromHaskell apiMinimizedRequested) >>= {#set weston_desktop_api->minimized_requested#} ptr 
    createSetXwaylandPositionFuncPtr (fromHaskell apiSetXwaylandPosition) >>= {#set weston_desktop_api->set_xwayland_position#} ptr 


{#pointer *weston_desktop as WestonDesktop newtype#}
{#fun weston_desktop_create {`WestonCompositor', `WestonDesktopApiPtr', `Ptr ()'} -> `WestonDesktop'#}

westonDesktopCreate :: WestonCompositor -> WestonDesktopApi a -> Ptr a -> IO WestonDesktop
westonDesktopCreate wc api udPtr = with api $ \apiPtr -> weston_desktop_create wc (castPtr apiPtr) (castPtr udPtr)

{#fun weston_desktop_destroy {`WestonDesktop'} -> `()'#}

{#fun weston_desktop_surface_get_surface {`WestonDesktopSurface'} -> `WestonSurface' #}
{#fun weston_surface_get_desktop_surface {`WestonSurface'} -> `WestonDesktopSurface' #}
{#fun weston_desktop_surface_set_size {`WestonDesktopSurface', `Int', `Int'} -> `()' #}

 
{#fun weston_desktop_surface_get_position_x {`WestonDesktopSurface'} -> `Int' #}
{#fun weston_desktop_surface_get_position_y {`WestonDesktopSurface'} -> `Int' #}
{#fun weston_desktop_surface_get_width {`WestonDesktopSurface'} -> `Int' #}
{#fun weston_desktop_surface_get_height {`WestonDesktopSurface'} -> `Int' #}

{#fun weston_desktop_surface_create_view {`WestonDesktopSurface'} -> `WestonView' #}
{#fun weston_desktop_surface_set_activated {`WestonDesktopSurface', `Bool'} -> `()' #}
