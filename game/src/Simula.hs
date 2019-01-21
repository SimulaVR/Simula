{-# LANGUAGE TypeApplications    #-}
module Simula (registerClasses) where

import           Godot.Extra.Register

import           Simula.GodotSimula
import           Simula.GodotSimulaController


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimula
  reg $ classInit @GodotSimulaController
