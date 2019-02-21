{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Plugin.Simula (GodotSimula(..)) where

import           Plugin.Imports

import           Plugin.Input
import           Plugin.Input.Grab
import           Plugin.SimulaController
import           Plugin.SimulaViewSprite
import           Plugin.VR

import           Godot.Core.GodotGlobalConstants
import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G


data GodotSimula = GodotSimula
  { _sObj      :: GodotObject
  , _sGrabState :: TVar GrabState
  }

instance GodotClass GodotSimula where
  godotClassName = "Simula"

instance ClassExport GodotSimula where
  classInit obj  = GodotSimula obj
    <$> newTVarIO NoGrab

  classExtends = "Node"
  classMethods =
    [ GodotMethod NoRPC "_ready" Plugin.Simula.ready
    , GodotMethod NoRPC "_process" process
    , GodotMethod NoRPC "on_button_signal" on_button_signal
    ]

instance HasBaseClass GodotSimula where
  type BaseClass GodotSimula = GodotNode
  super (GodotSimula obj _) = GodotNode obj


ready :: GFunc GodotSimula
ready self _ = undefined


on_button_signal :: GFunc GodotSimula
on_button_signal self args = undefined

onButton :: GodotSimula -> GodotSimulaController -> Int -> Bool -> IO ()
onButton self gsc button pressed = undefined

process :: GFunc GodotSimula
process self _ = undefined
