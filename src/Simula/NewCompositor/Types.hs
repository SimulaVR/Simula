module Simula.NewCompositor.Types where

import GHC.Exts (Constraint)

-- |Existential wrapper 
data Some :: (* -> Constraint) -> * where
  Some :: f a => { unSome :: a } -> Some f


