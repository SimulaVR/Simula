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
  )
where

import           Control.Concurrent.STM.TVar

import qualified Data.Text                     as T
import           Linear
import           Plugin.Imports
import           Godot.Nativescript

import           Godot.Extra.Register
import qualified Godot.Methods                 as G

import           Plugin.WestonSurfaceSprite
import           Plugin.Telekinesis
import           Plugin.Pointer

import           System.IO.Unsafe

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
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
    G.set_cast_to rc =<< toLowLevel (V3 0 0 (negate 10))
    G.set_enabled rc True
    G.add_child (GodotNode obj) (safeCast rc) True

    ctMesh <- unsafeInstance GodotMeshInstance "MeshInstance"
    G.set_skeleton_path ctMesh =<< toLowLevel ".."
    G.add_child (GodotNode obj) (safeCast ctMesh) True

    laser <- defaultPointer
    G.add_child (GodotNode obj) (safeCast laser) True

    let tf = TF (identity :: M33 Float) (V3 0 0 0)
    tk <- newTVarIO $ initTk (GodotSpatial obj) rc tf

    lsp <- newTVarIO 0

    G.set_visible (GodotSpatial obj) False

    return $ GodotSimulaController obj rc ctMesh (laser) tk lsp

  classExtends = "ARVRController"
  classMethods =
    [ GodotMethod NoRPC "_process" process
    , GodotMethod NoRPC "_physics_process" physicsProcess
    ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController       
  super (GodotSimulaController obj _ _ _ _ _) = GodotARVRController obj

load_controller_mesh :: Text -> IO (Maybe GodotMesh)
load_controller_mesh name = do
  msh <- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
    & unsafeNewNS GodotArrayMesh "ArrayMesh" []
  nameStr <- toLowLevel $ T.dropEnd 2 name
  ret <- G.call msh loadModelStr [toVariant (nameStr :: GodotString)] >>= fromGodotVariant
  if ret
    then
      return $ Just $ safeCast msh
    else do
      ret <- G.call msh loadModelStr [toVariant genericControllerStr] >>= fromGodotVariant
      if ret
        then return $ Just $ safeCast msh
        else return Nothing

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
          maxScale = 8

      if
        | norm curScale < minScale     -> resizeBy (V2 delta delta) obj
        | norm curScale > maxScale     -> resizeBy (V2 (-delta) (-delta)) obj
        | isGripPressed && validChange -> resizeBy diff obj
        | otherwise                    -> return ()

    Nothing -> return ()

  atomically $ writeTVar (_gscLastScrollPos ct) curPos

 where
  -- TODO: Implement proper resizing (not scaling), vert and horiz
  resizeBy :: (GodotSpatial :< child) => V2 Float -> child -> IO ()
  resizeBy (V2 x y) a =
    V3 1 1 1 ^* (1 + y * 0.5)
      & toLowLevel
      >>= G.scale_object_local (safeCast a :: GodotSpatial)


process :: GFunc GodotSimulaController
process self args = do
  delta <- getArg' 0 args :: IO Float
  active <- G.get_is_active self
  visible <- G.is_visible self

  if
    | not active -> G.set_visible self False
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
      mMesh <- load_controller_mesh cname
      case mMesh of
        Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
        Nothing   -> godotPrint "Failed to set controller mesh"
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
