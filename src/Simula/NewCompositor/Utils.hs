module Simula.NewCompositor.Utils where

import Control.Monad.Primitive
import qualified Data.Vector.Mutable as VM

snocM :: PrimMonad m => VM.MVector (PrimState m) a -> a -> m ()
snocM vec x = do
  let len = VM.length vec
  VM.grow vec (len + 1)
  VM.write vec len x
