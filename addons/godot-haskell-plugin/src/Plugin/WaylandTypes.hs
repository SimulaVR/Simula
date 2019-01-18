{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin.WaylandTypes where

import System.IO.Unsafe
import Foreign
--import Foreign.C
--import Foreign.Ptr
--import GHC.Real
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Language.C.Inline.Cpp as Cpp
import qualified Data.Map
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Text.RawString.QQ (r)

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
-- C.include "<wlr/types/wlr_xdg_shell.h>" -- nix presently lacks "xdg-shell-protocol.h"
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
initializeCppSimulaCtx = C.context $ Cpp.cppCtx <> Cpp.funCtx <> mempty {
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