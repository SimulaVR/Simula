{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Debug.C where

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
C.include "<stdio.h>"


-- Recall that to inline C++ Types of form "NameSpace::Type", you can use
-- C.verbatim "typedef NameSpace::Type Type;"

data C'WestonPointer

initializeCppSimulaCtx = C.context $ Cpp.cppCtx <> mempty {
  C.ctxTypesTable = Data.Map.fromList [
     -- AFAIK there is no easy way to add C2HS pointer types with to the inline-C type
     -- mapping table via, i.e.:
     -- (C.TypeName "weston_pointer", [t|C'WestonPointer|])

     (C.TypeName "weston_pointer", [t|C'WestonPointer|])
  ]
}
