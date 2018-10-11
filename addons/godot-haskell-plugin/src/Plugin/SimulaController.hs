{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Plugin.SimulaController
  ( GodotSimulaController(..)
  , isButtonPressed
  , pointerWindow
  ) where

import Control.Concurrent.STM.TVar

import qualified Data.Text                   as T
import           Linear
import           Plugin.Imports
import           Godot.Nativescript

import           Godot.Extra.Register
import qualified Godot.Methods               as G

import Plugin.WestonSurfaceSprite
import Plugin.Telekinesis

import System.IO.Unsafe

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscOpenvrMesh :: TVar (Maybe GodotArrayMesh)
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotMeshInstance
  , _gscTelekinesis :: TVar Telekinesis
  , _gscLastScrollPos :: TVar (V2 Float)
  }

instance Eq GodotSimulaController where
  (==) = (==) `on` _gscObj

instance GodotClass GodotSimulaController where
  godotClassName = "SimulaController"

instance ClassExport GodotSimulaController where
  classInit obj = do
    rc <- unsafeInstance GodotRayCast "RayCast"
    toLowLevel (V3 0 0 (negate 10)) >>= G.set_cast_to rc
    G.set_enabled rc True
    G.add_child (GodotNode obj) (safeCast rc) True

    mi <- unsafeInstance GodotMeshInstance "MeshInstance"
    G.add_child (GodotNode obj) (safeCast mi) True
    toLowLevel ".." >>= G.set_skeleton_path mi 

    laser <- "res://Laser.tscn"
      & sceneInstance 0 GodotMeshInstance "MeshInstance"

    G.add_child (GodotNode obj) (safeCast laser) True

    G.set_visible (GodotSpatial obj) False

    let tf = TF (identity :: M33 Float) (V3 0 0 0)
    tk <- newTVarIO $ initTk (GodotSpatial obj) rc tf

    lsp <- newTVarIO 0

    mesh <- newTVarIO Nothing
    return $ GodotSimulaController obj rc mesh mi (laser) tk lsp

  classExtends = "ARVRController"
  classMethods =
    [ GodotMethod NoRPC "_process" process
    , GodotMethod NoRPC "_physics_process" physicsProcess
    , GodotMethod NoRPC "_ready" ready
    ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj _ _ _ _ _ _) = GodotARVRController obj

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
          if ret
            then return $ Just $ safeCast msh
            else instance' GodotMesh "Mesh"

    Nothing -> return Nothing

 where
  loadModelStr, genericControllerStr :: GodotString
  loadModelStr = unsafePerformIO $ toLowLevel "load_model"
  {-# NOINLINE loadModelStr #-}
  genericControllerStr = unsafePerformIO $ toLowLevel "generic_controller"
  {-# NOINLINE genericControllerStr #-}

-- Because the ARVRController member method is_button_pressed returns Int, not Bool
isButtonPressed :: Int -> GodotSimulaController -> IO Bool
isButtonPressed btnId gsc = do
  ctId <- G.get_joystick_id $ (safeCast gsc :: GodotARVRController)
  getSingleton GodotInput "Input" >>= \inp -> G.is_joy_button_pressed inp ctId btnId

-- | Get the window pointed at if any.
pointerWindow :: GodotSimulaController -> IO (Maybe GodotWestonSurfaceSprite)
pointerWindow gsc = do
  isColliding <- G.is_colliding $ _gscRayCast gsc
  if isColliding
    then G.get_collider (_gscRayCast gsc) >>= tryObjectCast @GodotWestonSurfaceSprite
    else return Nothing

resize :: GodotSimulaController -> Float -> IO ()
resize ct delta = do
  curPos <- V2 <$> (ct `G.get_joystick_axis` 0) <*> (ct `G.get_joystick_axis` 1)

  _tkBody <$> (readTVarIO (_gscTelekinesis ct)) >>= \case
    Just (obj, _) -> do
      isGripPressed <- isButtonPressed 2 ct
      lastPos <- readTVarIO (_gscLastScrollPos ct)
      curScale <- G.get_scale (safeCast obj :: GodotSpatial) >>= fromLowLevel

      let diff = curPos - lastPos
          validChange = norm lastPos > 0.01 && norm curPos > 0.01
          minScale = 1
          maxScale = 4

      if | norm curScale < minScale     -> scaleBy (V2 delta delta) obj
         | norm curScale > maxScale     -> scaleBy (V2 (-delta) (-delta)) obj
         | isGripPressed && validChange -> scaleBy diff obj
         | otherwise                    -> return ()

    Nothing -> return ()

  atomically $ writeTVar (_gscLastScrollPos ct) curPos

 where
  -- TODO: Implement proper resizing (not scaling), vert and horiz
  scaleBy :: (GodotSpatial :< child) => V2 Float -> child -> IO ()
  scaleBy (V2 x y) a =
    V3 1 1 1 ^* (1 + y * 0.5)
      & toLowLevel
      >>= G.scale_object_local (safeCast a :: GodotSpatial)


process :: GFunc GodotSimulaController
process self args = do
  delta <- getArg' 0 args :: IO Float
  active <- G.get_is_active self
  visible <- G.is_visible self

  if | not active -> G.set_visible self False
     | visible -> do
         resize self delta
         pointerWindow self >>= \case
           Just window -> do
             G.set_visible (_gscLaser self) True
             pos <- G.get_collision_point $ _gscRayCast self
             processClickEvent window Motion pos
           Nothing -> do
             G.set_visible (_gscLaser self) False
             return ()
     | otherwise -> do
         cname <- G.get_controller_name self >>= fromLowLevel
         mMesh <- load_controller_mesh self  cname
         case mMesh of
           Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
           Nothing -> return ()
         G.set_visible self True

  toLowLevel VariantNil


physicsProcess :: GFunc GodotSimulaController
physicsProcess self _ = do
  whenM (G.get_is_active self) $ do
    isGripPressed <- isButtonPressed 2 self
    triggerPull <- G.get_joystick_axis self 2
    let levitateCond = isGripPressed -- && triggerPull > 0.01
    let moveCond = triggerPull > 0.2

    tk <- readTVarIO (_gscTelekinesis self) >>= telekinesis levitateCond moveCond
    atomically $ writeTVar (_gscTelekinesis self) tk

  retnil


ready :: GFunc GodotSimulaController
ready self _ = do
  -- Load and set controller mesh
  mesh <- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
    & newNS GodotArrayMesh "ArrayMesh" []
  atomically $ writeTVar (_gscOpenvrMesh self) $ Just mesh

  toLowLevel VariantNil
