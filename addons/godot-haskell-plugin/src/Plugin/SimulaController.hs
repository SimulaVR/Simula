{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugin.SimulaController
  ( GodotSimulaController(..)

  ) where

import Simula.WaylandServer
import Simula.Weston

import Control.Monad
import Data.Coerce

import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports

import Godot.Gdnative.Internal.Api
import           Godot.Gdnative.Types        (GodotFFI, LibType, TypeOf)
import qualified Godot.Methods               as G

import qualified Godot.Core.GodotImage as Image

import Godot.Core.GodotGlobalConstants

import Plugin.WestonSurfaceSprite

import Control.Lens

import Foreign

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  }

instance GodotClass GodotSimulaController where
  godotClassName = "SimulaController"

instance ClassExport GodotSimulaController where
  classInit obj = do
    rc <- GodotRayCast <$> mkClassInstance "RayCast"
    toLowLevel (V3 0 0 (negate 1)) >>= G.set_cast_to rc
    G.set_enabled rc True
    G.add_child (GodotNode obj) (safeCast rc) True

    return $ GodotSimulaController obj rc
  classExtends = "ARVRController"
  classMethods = []

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj _) = GodotARVRController obj

