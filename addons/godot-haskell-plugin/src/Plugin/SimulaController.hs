{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Plugin.SimulaController
  ( GodotSimulaController(..)

  ) where

import Simula.WaylandServer
import Simula.Weston

import Control.Concurrent.STM.TVar
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
  , _gscOpenvrMesh :: TVar (Maybe GodotArrayMesh)
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotNode
  }

instance Eq GodotSimulaController where
  (==) = (==) `on` _gscObj

instance GodotClass GodotSimulaController where
  godotClassName = "SimulaController"

instance ClassExport GodotSimulaController where
  classInit obj = do
    rc <- GodotRayCast <$> mkClassInstance "RayCast"
    toLowLevel (V3 0 0 (negate 10)) >>= G.set_cast_to rc
    G.set_enabled rc True
    G.add_child (GodotNode obj) (safeCast rc) True

    mi <- GodotMeshInstance <$> mkClassInstance "MeshInstance"
    G.add_child (GodotNode obj) (safeCast mi) True
    toLowLevel ".." >>= G.set_skeleton_path mi 

    rl <- getResourceLoader
    url <- toLowLevel "res://Laser.tscn"
    typeHint <- toLowLevel ""
    (GodotResource laserObj) <- G.load rl url typeHint False
    let laserScene = GodotPackedScene laserObj
    laser <- G.instance' laserScene 0

    G.add_child (GodotNode obj) (safeCast laser) True

    G.set_visible (GodotSpatial obj) False

    mesh <- newTVarIO Nothing
    return $ GodotSimulaController obj rc mesh mi laser

  classExtends = "ARVRController"
  classMethods =
    [ Func NoRPC "_process" process
    , Func NoRPC "_ready" ready
    ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj  _ _ _ _) = GodotARVRController obj

load_controller_mesh :: GodotSimulaController -> Text -> IO (Maybe GodotMesh)
load_controller_mesh gsc name = do
  nameStr <- toLowLevel $ T.dropEnd 2 name
  mMsh <- readTVarIO (_gscOpenvrMesh gsc)
  case mMsh of
    Just msh -> do
      ret <- G.call msh loadModelStr [toVariant (nameStr :: GodotString)] >>= fromGodotVariant
      if ret
        then
          return $ Just $ safeCast msh
        else do
          ret <- G.call msh loadModelStr [toVariant genericControllerStr] >>= fromGodotVariant
          if ret then return $ Just $ safeCast msh
          else Just <$> GodotMesh <$> mkClassInstance "Mesh"

    Nothing -> return Nothing

 where
  loadModelStr, genericControllerStr :: GodotString
  loadModelStr = unsafePerformIO $ toLowLevel "load_model"
  {-# NOINLINE loadModelStr #-}
  genericControllerStr = unsafePerformIO $ toLowLevel "generic_controller"
  {-# NOINLINE genericControllerStr #-}


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
    mMesh <- load_controller_mesh self  cname
    case mMesh of
      Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
      Nothing -> return ()
    G.set_visible self True
  toLowLevel VariantNil

ready :: GodotFunc GodotSimulaController
ready _ self _ = do
  -- Load and set controller mesh
  rl <- getResourceLoader
  url <- toLowLevel "res://addons/godot-openvr/OpenVRRenderModel.gdns"
  typeHint <- toLowLevel ""
  (GodotResource obj) <- G.load rl url typeHint False
  let ns = GodotNativeScript obj
  mesh <- GodotArrayMesh <$> G.new ns []
  atomically $ writeTVar (_gscOpenvrMesh self) $ Just mesh

  toLowLevel VariantNil
