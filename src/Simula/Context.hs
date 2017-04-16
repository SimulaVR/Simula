{-# LANGUAGE OverloadedStrings #-}
module Simula.Context where

import qualified Data.Map as Map
import Data.Monoid
import Language.C.Inline.Context
import Language.C.Types
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

import Simula.Compositor.Types

simulaCtx :: Context
simulaCtx = Cpp.cppCtx <> C.vecCtx <> mempty {
  ctxTypesTable = Map.fromList [
      (TypeName "Scene", [t|Scene|]),
      (TypeName "Compositor", [t|Compositor|]),
      (TypeName "Display", [t|Display|]),
      (TypeName "WindowManager", [t|WindowManager|]),
      (TypeName "OpenGLContext", [t|OpenGLContext|]),
      (TypeName "Skeleton", [t|Skeleton|]),
      (TypeName "Seat", [t|Seat|])
      ] }
