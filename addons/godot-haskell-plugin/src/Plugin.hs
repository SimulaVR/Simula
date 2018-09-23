{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Godot.Extra.Register

import           Plugin.SimulaController
import           Plugin.Weston
import           Plugin.WestonSurfaceSprite
import           Plugin.WestonSurfaceTexture


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimulaController
  reg $ classInit @GodotWestonCompositor
  reg $ classInit @GodotWestonSurfaceSprite
  reg $ classInit @GodotWestonSurfaceTexture
