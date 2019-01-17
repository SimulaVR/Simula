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

C.include "<wayland-server.h>"
-- C.include "<wlr/backend.h>"              -- TODO: Fix udev error
-- C.include "<wlr/render/wlr_renderer.h>"  -- TODO: Fix -WLR_USEUUNSTABLE error (nix not recognizing this is set to true in meson.build)
-- C.include "<wlr/types/wlr_cursor.h>"
-- C.include "<wlr/types/wlr_compositor.h>"
-- C.include "<wlr/types/wlr_data_device.h>"
-- C.include "<wlr/types/wlr_input_device.h>"
-- C.include "<wlr/types/wlr_keyboard.h>"
-- C.include "<wlr/types/wlr_matrix.h>"
-- C.include "<wlr/types/wlr_output.h>"
-- C.include "<wlr/types/wlr_output_layout.h>"
-- C.include "<wlr/types/wlr_pointer.h>"
-- C.include "<wlr/types/wlr_seat.h>"
-- C.include "<wlr/types/wlr_xcursor_manager.h>"
-- C.include "<wlr/types/wlr_xdg_shell.h>"
-- C.include "<wlr/util/log.h>"
-- C.include "<xkbcommon/xkbcommon.h>"
-- C.include "<weston/weston.h>"
-- C.include "<...wlroots...>"

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

-- All C types are kept in an explicit marshalling table.
initializeCppSimulaCtx = C.context $ Cpp.cppCtx <> Cpp.funCtx <> mempty {
  C.ctxTypesTable = Data.Map.fromList [
     (C.TypeName "wl_display", [t|C'WlDisplay|])
     -- (C.TypeName "wl_shm_format_argb8888", [t|C'WlShmFormatArgb8888|])
     -- (C.TypeName "wl_keyboard_key_state", [t|C'WlKeyboardKeyState|])
  ]
}