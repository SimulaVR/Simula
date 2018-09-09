{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
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
import Plugin.Telekinesis

import Control.Lens

import Foreign

import System.IO.Unsafe

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscOpenvrMesh :: TVar (Maybe GodotArrayMesh)
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotMeshInstance
  , _gscTelekinesis :: TVar Telekinesis
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

    tf <- G.get_global_transform (GodotSpatial obj) >>= fromLowLevel
    tk <- newTVarIO $ initTk (GodotSpatial obj) rc tf

    mesh <- newTVarIO Nothing
    return $ GodotSimulaController obj rc mesh mi (GodotMeshInstance $ safeCast laser) tk

  classExtends = "ARVRController"
  classMethods =
    [ Func NoRPC "_process" process
    , Func NoRPC "_physics_process" physicsProcess
    , Func NoRPC "_ready" ready
    ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj _ _ _ _ _) = GodotARVRController obj

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

  if | not active -> G.set_visible self False
     | visible -> do
         isColliding <- G.is_colliding (_gscRayCast self)
         if | isColliding ->
                G.get_collider (_gscRayCast self)
                  >>= tryObjectCast @GodotWestonSurfaceSprite
                  >>= \case
                    Just window -> do
                      G.set_visible (_gscLaser self) True
                      pos <- G.get_collision_point (_gscRayCast self)
                      processClickEvent window Motion pos
                    Nothing -> do
                      G.set_visible (_gscLaser self) False
                      return ()
            | otherwise -> return ()
     | otherwise -> do
         cname <- G.get_controller_name self >>= fromLowLevel
         mMesh <- load_controller_mesh self  cname
         case mMesh of
           Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
           Nothing -> return ()
         G.set_visible self True
  toLowLevel VariantNil


physicsProcess :: GodotFunc GodotSimulaController
physicsProcess _ self _ = do
  btnId <- G.get_joystick_id $ (safeCast self :: GodotARVRController)

  isGripPressed <- getInput >>= \inp -> G.is_joy_button_pressed inp btnId 2
  triggerPull <- G.get_joystick_axis self 2
  let levitateCond = isGripPressed && triggerPull > 0.01
  let moveCond = triggerPull > 0.2

  tk <- readTVarIO (_gscTelekinesis self) >>= telekinesis levitateCond moveCond
  atomically $ writeTVar (_gscTelekinesis self) tk

  retnil


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
