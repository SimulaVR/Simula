{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug.C where

import System.IO.Unsafe
import Foreign
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.Map
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Text.RawString.QQ (r)

import Graphics.Wayland.Server
import Graphics.Wayland.Internal.Server
import Graphics.Wayland.WlRoots.Compositor
import Graphics.Wayland.WlRoots.Output
import Graphics.Wayland.WlRoots.Surface
import Graphics.Wayland.WlRoots.Backend

import Data.Coerce

-- HsRoots Modules:
{-
import      Graphics.Egl
import      Graphics.Pixman
import      Graphics.Wayland.Global
import      Graphics.Wayland.List
import      Graphics.Wayland.Resource
import      Graphics.Wayland.Server.Client
import      Graphics.Wayland.Signal
import      Graphics.Wayland.WlRoots.Backend
import      Graphics.Wayland.WlRoots.Backend.Headless
import      Graphics.Wayland.WlRoots.Backend.Libinput
import      Graphics.Wayland.WlRoots.Backend.Multi
import      Graphics.Wayland.WlRoots.Backend.Session
import      Graphics.Wayland.WlRoots.Box
import      Graphics.Wayland.WlRoots.Buffer
import      Graphics.Wayland.WlRoots.Compositor
import      Graphics.Wayland.WlRoots.Cursor
import      Graphics.Wayland.WlRoots.DeviceManager
import      Graphics.Wayland.WlRoots.ExportDMABuf
import      Graphics.Wayland.WlRoots.Egl
import      Graphics.Wayland.WlRoots.GammaControl
import      Graphics.Wayland.WlRoots.Global
import      Graphics.Wayland.WlRoots.IdleInhibit
import      Graphics.Wayland.WlRoots.Input
import      Graphics.Wayland.WlRoots.Input.Buttons
import      Graphics.Wayland.WlRoots.Input.Keyboard
import      Graphics.Wayland.WlRoots.Input.Pointer
import      Graphics.Wayland.WlRoots.Input.Tablet
import      Graphics.Wayland.WlRoots.Input.TabletPad
import      Graphics.Wayland.WlRoots.Input.TabletTool
import      Graphics.Wayland.WlRoots.Input.Touch
import      Graphics.Wayland.WlRoots.InputInhibitor
import      Graphics.Wayland.WlRoots.LinuxDMABuf
import      Graphics.Wayland.WlRoots.Output
import      Graphics.Wayland.WlRoots.OutputLayout
import      Graphics.Wayland.WlRoots.PrimarySelection
import      Graphics.Wayland.WlRoots.Render
import      Graphics.Wayland.WlRoots.Render.Color
import      Graphics.Wayland.WlRoots.Render.Gles2
import      Graphics.Wayland.WlRoots.Render.Matrix
import      Graphics.Wayland.WlRoots.Screenshooter
import      Graphics.Wayland.WlRoots.Seat
import      Graphics.Wayland.WlRoots.ServerDecoration
import      Graphics.Wayland.WlRoots.Surface
import      Graphics.Wayland.WlRoots.SurfaceLayers
import      Graphics.Wayland.WlRoots.Tabletv2
import      Graphics.Wayland.WlRoots.Util
import      Graphics.Wayland.WlRoots.Util.Region
import      Graphics.Wayland.WlRoots.WlShell
import      Graphics.Wayland.WlRoots.XCursor
import      Graphics.Wayland.WlRoots.XCursorManager
import      Graphics.Wayland.WlRoots.XWayland
import      Graphics.Wayland.WlRoots.XdgShell
import      Graphics.Wayland.WlRoots.XdgShellv6
-}

C.verbatim "#define WLR_USE_UNSTABLE" -- Until wlroots stabalizes their API, we have to put this at the top of all of our modules that make use of wlr_* types
C.include "<wayland-server.h>"
C.include "<wlr/backend.h>"
C.include "<wlr/render/wlr_renderer.h>"
C.include "<wlr/types/wlr_cursor.h>"
C.include "<wlr/types/wlr_compositor.h>"
C.include "<wlr/types/wlr_data_device.h>"
C.include "<wlr/types/wlr_input_device.h>"
C.include "<wlr/types/wlr_keyboard.h>"
C.include "<wlr/types/wlr_matrix.h>"
C.include "<wlr/types/wlr_output.h>"
C.include "<wlr/types/wlr_output_layout.h>"
C.include "<wlr/types/wlr_pointer.h>"
C.include "<wlr/types/wlr_seat.h>"
C.include "<wlr/types/wlr_xcursor_manager.h>"
C.include "<wlr/util/log.h>"
C.include "<xkbcommon/xkbcommon.h>"

-- These denote actual structs, but are opaque in Haskell without explicit Storable declarations
data C'WlDisplay
data C'WlShmFormatArgb8888
data C'WlKeyboardKeyState

-- Speculative list of needed types.
data C'WlrSurface
data C'WlrCompositor
data C'WlrView
data C'WlrOutput
data C'WlrKeyboard
data C'WlrPointer
data C'WlrSeat
data C'WlrDesktopApi
data C'WlrLayer
data C'WlrBackend
data C'WlrButton
data C'WlEventLoop
data C'WlListener


-- typedef void(* wl_notify_func_t) (struct wl_listener *listener, void *data)
type NotifyFuncT = (Ptr C'WlListener -> Ptr ())
foreign import ccall "wrapper" mkNotifyFuncT :: NotifyFuncT -> IO (FunPtr NotifyFuncT)

-- All C types are kept in an explicit marshalling table.
-- Note we must not use C++ in this project as wlroots seems incompatible with C++ compilers (see wlr_renderer.h use of [static ..])
initializeSimulaCtx = C.context $ C.baseCtx <> C.funCtx <> mempty {
  C.ctxTypesTable = Data.Map.fromList [
     (C.TypeName "wl_display", [t|C'WlDisplay|])
  ,  (C.TypeName "wlr_surface", [t|C'WlrSurface|])
  ,  (C.TypeName "wlr_compositor", [t|C'WlrCompositor|])
  ,  (C.TypeName "wlr_view", [t|C'WlrView|])
  ,  (C.TypeName "wlr_output", [t|C'WlrOutput|])
  ,  (C.TypeName "wlr_keyboard", [t|C'WlrKeyboard|])
  ,  (C.TypeName "wlr_pointer", [t|C'WlrPointer|])
  ,  (C.TypeName "wlr_seat", [t|C'WlrSeat|])
  ,  (C.TypeName "wlr_desktopApi", [t|C'WlrDesktopApi|])
  ,  (C.TypeName "wlr_layer", [t|C'WlrLayer|])
  ,  (C.TypeName "wlr_backend", [t|C'WlrBackend|])
  ,  (C.TypeName "wlr_button", [t|C'WlrButton|])
  ,  (C.TypeName "wl_event_loop", [t|C'WlEventLoop|])

--  ,  (C.TypeName "wl_notify_func_t", [t|C'NotifyFuncT|])
     -- (C.TypeName "wl_shm_format_argb8888", [t|C'WlShmFormatArgb8888|])
     -- (C.TypeName "wl_keyboard_key_state", [t|C'WlKeyboardKeyState|])
  ]
}

-- | Helper function to quickly load inline-C state at the top of a module.
initializeSimulaCtxAndIncludes = do
  initializeSimulaCtx
  C.verbatim "#define WLR_USE_UNSTABLE" -- Until wlroots stabalizes their API, we have to put this at the top of all of our modules that make use of wlr_* types
  C.include "<wayland-server.h>"
  C.include "<wlr/backend.h>"
  C.include "<wlr/render/wlr_renderer.h>"
  C.include "<wlr/types/wlr_cursor.h>"
  C.include "<wlr/types/wlr_compositor.h>"
  C.include "<wlr/types/wlr_data_device.h>"
  C.include "<wlr/types/wlr_input_device.h>"
  C.include "<wlr/types/wlr_keyboard.h>"
  C.include "<wlr/types/wlr_matrix.h>"
  C.include "<wlr/types/wlr_output.h>"
  C.include "<wlr/types/wlr_output_layout.h>"
  C.include "<wlr/types/wlr_pointer.h>"
  C.include "<wlr/types/wlr_seat.h>"
  C.include "<wlr/types/wlr_xcursor_manager.h>"
  C.include "<wlr/util/log.h>"
  C.include "<xkbcommon/xkbcommon.h>"

-- |FFI is a relation between C types marshalled from inline-C
-- |and C2HS that helps you cast types from one service to the other.
class FFI inlinec chs | inlinec -> chs where
  toC2HS :: inlinec -> chs
  toInlineC :: chs -> inlinec

-- |Here we convert from inline-C to C2HS the safe way (via data constructor pattern matching).
instance FFI (Ptr C'WlDisplay) DisplayServer where
  toC2HS ptrToWlDisplay = (DisplayServer ((castPtr ptrToWlDisplay) :: Ptr DisplayServer))
  toInlineC (DisplayServer ptrToDisplayServer) = (castPtr ptrToDisplayServer) :: Ptr C'WlDisplay

-- | Below we convert via `coerce` (and assume that GHC knows what to do automatically):
-- |"Any two nominally equal types are also representationally equal, and in addition, a newtype is
-- | representationally equal to its underlying type. For example, GHC knows that Int is
-- | representationally equal to Int, and if you have newtype WrappedInt = WrapInt Int,
-- | then GHC knows that Int is representationally equal to WrappedInt." -- /r/haskell
instance FFI (Ptr C'WlEventLoop) EventLoop where
 toC2HS ptrToWlEventLoop = (EventLoop ((castPtr ptrToWlEventLoop) :: Ptr EventLoop))
 toInlineC (EventLoop ptrToEventLoop) = (castPtr ptrToEventLoop) :: Ptr C'WlEventLoop

instance FFI (Ptr C'WlrCompositor) (Ptr WlrCompositor) where
  toC2HS ptrToWlrCompositor = (castPtr ptrToWlrCompositor) :: Ptr WlrCompositor
  toInlineC ptrToWlrCompositor = (castPtr ptrToWlrCompositor) :: Ptr C'WlrCompositor

instance FFI (Ptr C'WlrBackend) (Ptr Backend) where
  toC2HS ptrToWlrBackend = (castPtr ptrToWlrBackend) :: Ptr Backend
  toInlineC ptrToBackend = (castPtr ptrToBackend) :: Ptr C'WlrBackend

instance FFI (Ptr C'WlrSurface) (Ptr WlrSurface) where
  toC2HS ptrToWlrSurface = (castPtr ptrToWlrSurface) :: Ptr WlrSurface
  toInlineC ptrToWlrSurface = (castPtr ptrToWlrSurface) :: Ptr C'WlrSurface

instance FFI (Ptr C'WlrOutput) (Ptr WlrOutput) where
  toC2HS ptrToWlrOutput = (castPtr ptrToWlrOutput) :: Ptr WlrOutput
  toInlineC ptrToWlrOutput = (castPtr ptrToWlrOutput) :: Ptr C'WlrOutput

-- |hsroots doesn't seem to have a type corresponding to a `* wlr_layer`
-- instance FFI (Ptr C'WlrLayer) _ where

-- |hsroots doesn't seem to have a type corresponding to a `* wl_listener`
-- instance FFI (Ptr C'WlListener) _ where

-- |hsroots doesn't seem to have a type corresponding to a `* wl_notify_func_t`
-- instance FFI NotifyFuncT _ where