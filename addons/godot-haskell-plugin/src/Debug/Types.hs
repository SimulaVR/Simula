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