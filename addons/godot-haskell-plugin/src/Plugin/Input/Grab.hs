{-# LANGUAGE LambdaCase #-}
module Plugin.Input.Grab where

import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Plugin.Imports
import           Plugin.SimulaController
import           Plugin.SimulaViewSprite
import           Plugin.SimulaViewTexture


data GrabState
  = NoGrab
  | Manipulating (GodotSimulaController, GodotSimulaViewSprite)
  | ManipulatingDual
      (GodotSimulaController, GodotSimulaViewSprite)
      (GodotSimulaController, GodotSimulaViewSprite)
  | Resizing (GodotSimulaController, GodotSimulaController) GodotSimulaViewSprite Float -- dist


processGrabEvent
  :: GodotSimulaController
  -> Maybe GodotSimulaViewSprite
  -> Bool
  -> GrabState
  -> IO GrabState
processGrabEvent gsc maybeWindow pressed = undefined

handleState :: GrabState -> IO GrabState
handleState = undefined
