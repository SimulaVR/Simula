module Simula.NewCompositor.Utils where

import Linear

scale :: Floating a => M44 a -> V3 a -> M44 a
scale mat scale = fmap (liftI2 (*) (point scale)) mat
