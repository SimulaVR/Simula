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

import Simula.Weston

C.include "<wayland-server.h>"
C.include "<weston/weston.h>"
-- C.include "<...wlroots...>"

-- These denote actual structs, but are opaque in Haskell without explicit Storable declarations
data C'WlDisplay
data C'WlShmFormatArgb8888
data C'WlKeyboardKeyState

-- Speculative list of needed types.
data C'WlrSurface
data C'WlrView
data C'WlrOuput
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
     (C.TypeName "wl_shm_format_argb8888", [t|C'WlShmFormatArgb8888|])
     (C.TypeName "wl_keyboard_key_state", [t|C'WlKeyboardKeyState|])
  ]
}