{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Godot.Extra.Register

import           Plugin.Simula
import           Plugin.SimulaController


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimula
  reg $ classInit @GodotSimulaController
