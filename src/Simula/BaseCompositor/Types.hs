module Simula.BaseCompositor.Types where

import GHC.Exts (Constraint)

-- |Existential wrapper 
data Some :: (* -> Constraint) -> * where
  Some :: f a => a -> Some f


