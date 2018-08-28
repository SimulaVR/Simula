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

import System.IO.Unsafe

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscOpenvrMesh :: GodotArrayMesh
  , _gscMeshInstance :: GodotMeshInstance
  }

instance GodotClass GodotSimulaController where
  godotClassName = "SimulaController"

instance ClassExport GodotSimulaController where
  classInit obj = do
    rc <- GodotRayCast <$> mkClassInstance "RayCast"
    toLowLevel (V3 0 0 (negate 1)) >>= G.set_cast_to rc
    G.set_enabled rc True
    G.add_child (GodotNode obj) (safeCast rc) True

    mi <- GodotMeshInstance <$> mkClassInstance "MeshInstance"
    G.add_child (GodotNode obj) (safeCast mi) True
    toLowLevel ".." >>= G.set_skeleton_path mi 

    rl <- getResourceLoader
    url <- toLowLevel "res://addons/godot-openvr/OpenVRRenderModel.gdns"
    typeHint <- toLowLevel ""
    (GodotResource obj) <- G.load rl url typeHint False
    let ns = GodotNativeScript obj
    mesh <- G.new ns []

    G.set_visible (GodotSpatial obj) False

    return $ GodotSimulaController obj rc (GodotArrayMesh mesh) mi
  classExtends = "ARVRController"
  classMethods = [ Func NoRPC "_process" process ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj  _ _ _) = GodotARVRController obj



load_controller_mesh :: GodotSimulaController -> Text -> IO GodotMesh
load_controller_mesh gsc name = do
  nameStr <- toLowLevel $ T.dropEnd 2 name 
  ret <- G.call (_gscOpenvrMesh gsc) loadModelStr [toVariant (nameStr :: GodotString)] >>= fromGodotVariant
  if ret then return $ safeCast (_gscOpenvrMesh gsc)
  else do
    ret <- G.call (_gscOpenvrMesh gsc) loadModelStr [toVariant genericControllerStr] >>= fromGodotVariant
    if ret then return $ safeCast (_gscOpenvrMesh gsc)
    else newMesh


  where
    loadModelStr, genericControllerStr :: GodotString
    loadModelStr = unsafePerformIO $ toLowLevel "load_model"
    {-# NOINLINE loadModelStr #-}
    genericControllerStr = unsafePerformIO $ toLowLevel "generic_controller"
    {-# NOINLINE genericControllerStr #-}

    newMesh = GodotMesh <$> mkClassInstance "Mesh"


process :: GodotFunc GodotSimulaController
process _ self _ = do
  active <- G.get_is_active self
  visible <- G.is_visible self

  if not active then do
    G.set_visible self False
  else if visible then
    return ()
  else do
    cname <- G.get_controller_name self >>= fromLowLevel
    mesh <- load_controller_mesh self  cname
    G.set_mesh (_gscMeshInstance self) mesh
    G.set_visible self True
  toLowLevel VariantNil

