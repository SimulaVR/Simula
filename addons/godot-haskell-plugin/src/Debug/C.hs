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
import      Graphics.Wayland.Signal

import Data.Coerce


-- |These hold actual structs, but are opaque to Haskell without explicit Storable declarations.
data C'WlDisplay
data C'WlList
-- data C'WlShmFormatArgb8888 -- Probably will be needed for transfering wayland buffers to Godot
-- data C'WlKeyboardKeyState  -- Used in old Simula code
data C'WlrSurface
data C'WlrCompositor
data C'WlrView
data C'WlrOutput
data C'WlrOutputLayout
data C'WlrKeyboard
data C'WlrPointer
data C'WlrDesktopApi
data C'WlrLayer
data C'WlrBackend
data C'WlrButton
data C'WlEventLoop
data C'WlrRenderer
data C'WlrXdgShell
data C'WlrCursor
data C'WlrXCursorManager
data C'WlrSeat
data C'WlrOuutputLayout
data C'WlrInputDevice
data C'WlListener  -- HsRoots treats this type weirdly; see Marshal.hs

-- |Here we enrich C'WlNotifyFuncT with some inner structure. Note that in C:
-- |typedef void(* wl_notify_func_t) (struct wl_listener *listener, void *data)
type C'WlNotifyFuncT = FunPtr (Ptr C'WlListener -> Ptr () -> IO ())

-- |All inline-C types are kept in an explicit marshalling table.
-- |Note we must not use C++ in this project as wlroots is incompatible with C++ compilers (see wlr_renderer.h use of [static ..]).
initializeSimulaCtx = C.context $ C.baseCtx <> C.funCtx <> mempty {
  C.ctxTypesTable = Data.Map.fromList [
     (C.Struct "wl_display", [t|C'WlDisplay|])
  ,  (C.Struct "wl_listener", [t|C'WlListener|])
  ,  (C.Struct "wl_event_loop", [t|C'WlEventLoop|])
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
  ,  (C.TypeName "wl_notify_func_t", [t|C'WlNotifyFuncT|])
  ,  (C.TypeName "wlr_renderer", [t|C'WlrRenderer|])
  ,  (C.TypeName "wlr_cursor", [t|C'WlrCursor|])
  ,  (C.TypeName "wlr_xcursor_manager", [t|C'WlrXCursorManager|])
  ,  (C.TypeName "wlr_seat", [t|C'WlrSeat|])
  ,  (C.TypeName "wlr_output_layout", [t|C'WlrOutputLayout|])
  ,  (C.TypeName "wlr_input_device", [t|C'WlrInputDevice|])
-- (C.TypeName "wl_shm_format_argb8888", [t|C'WlShmFormatArgb8888|])
-- (C.TypeName "wl_keyboard_key_state", [t|C'WlKeyboardKeyState|])
  ]
}

-- |Helper function to quickly load inline-C state at the top of a module.
initializeSimulaCtxAndIncludes = do
  initializeSimulaCtx
  C.verbatim "#define WLR_USE_UNSTABLE" -- Until wlroots stabalizes their API, we have to put this at the top of all of our modules that make use of wlr_* types
  C.include "<wayland-server.h>"
  C.include "<wayland-server-core.h>"
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
  C.include "<assert.h>"
  C.include "<time.h>"
