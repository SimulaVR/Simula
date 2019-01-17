{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Godot.Extra.Register

import           Plugin.Simula
import           Plugin.SimulaController
import           Plugin.Wlroots
import           Plugin.WlrootsSurfaceSprite
import           Plugin.WlrootsSurfaceTexture


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimula
  reg $ classInit @GodotSimulaController
  reg $ classInit @GodotWlrootsCompositor
  reg $ classInit @GodotWlrootsSurfaceSprite
  reg $ classInit @GodotWlrootsSurfaceTexture
