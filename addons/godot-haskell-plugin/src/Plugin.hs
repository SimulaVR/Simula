{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Godot.Nativescript

import           Plugin.Simula
import           Plugin.SimulaController
import           Plugin.SimulaServer
import           Plugin.SimulaViewSprite
import           Plugin.Types
import           Plugin.PancakeCamera
import           Plugin.CanvasAR


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimula
  reg $ classInit @GodotSimulaController
  reg $ classInit @GodotSimulaServer
  reg $ classInit @GodotSimulaViewSprite
  reg $ classInit @CanvasBase
  reg $ classInit @CanvasSurface
  reg $ classInit @GodotPancakeCamera
  reg $ classInit @CanvasAR
