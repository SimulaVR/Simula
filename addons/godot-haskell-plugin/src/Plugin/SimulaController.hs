{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Plugin.SimulaController
  ( GodotSimulaController(..)
  , addSimulaController
  , isButtonPressed
  , pointerWindow
  )
where

import           Control.Concurrent.STM.TVar

import qualified Data.Text                     as T
import           Linear

import           Plugin.Imports
import           Plugin.SimulaViewTexture
import           Plugin.SimulaViewSprite
import           Plugin.Input.Telekinesis
import           Plugin.Pointer

import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Foreign ( deRefStablePtr
                         , castPtrToStablePtr
                         )

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotMeshInstance
  , _gscTelekinesis :: TVar Telekinesis
  , _gscLastScrollPos :: TVar (V2 Float)
  }

-- instance Eq GodotSimulaController where
--   (==) = (==) `on` _gscObj

-- instance GodotClass GodotSimulaController where
--   godotClassName = "SimulaController"

-- instance ClassExport GodotSimulaController where
--   classInit obj = do
--     rc <- unsafeInstance GodotRayCast "RayCast"
--     G.set_cast_to rc =<< toLowLevel (V3 0 0 (negate 10))
--     G.set_enabled rc True
--     G.add_child (GodotNode obj) (safeCast rc) True

--     ctMesh <- unsafeInstance GodotMeshInstance "MeshInstance"
--     G.set_skeleton_path ctMesh =<< toLowLevel ".."
--     G.add_child (GodotNode obj) (safeCast ctMesh) True

--     laser <- defaultPointer
--     G.add_child (GodotNode obj) (safeCast laser) True

--     let tf = TF (identity :: M33 Float) (V3 0 0 0)
--     tk <- newTVarIO $ initTk (GodotSpatial obj) rc tf

--     lsp <- newTVarIO 0

--     G.set_visible (GodotSpatial obj) False

--     return $ GodotSimulaController
--       { _gscObj           = obj
--       , _gscRayCast       = rc
--       , _gscMeshInstance  = ctMesh
--       , _gscLaser         = laser
--       , _gscTelekinesis   = tk
--       , _gscLastScrollPos = lsp
--       }

--   classExtends = "ARVRController"
--   classMethods =
--     [ GodotMethod NoRPC "_process" process
--     , GodotMethod NoRPC "_physics_process" physicsProcess
--     ]

-- instance HasBaseClass GodotSimulaController where
--   type BaseClass GodotSimulaController = GodotARVRController
--   super (GodotSimulaController obj _ _ _ _ _) = GodotARVRController obj


loadOpenVRControllerMesh :: Text -> IO (Maybe GodotMesh)
loadOpenVRControllerMesh name = undefined
  -- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
  --   & newNS GodotArrayMesh "ArrayMesh" [] >>= \case
  --     Nothing  ->
  --       Nothing <$ godotPrint "Couldn't find an OpenVR render model."

  --     Just msh -> do
  --       loadModelStr <- toLowLevel "load_model"
  --       nameStr :: GodotString <- toLowLevel $ T.dropEnd 2 name
  --       ret <- G.call msh loadModelStr [toVariant nameStr] >>= fromGodotVariant
  --       if ret
  --         then return $ Just $ safeCast msh
  --         else do
  --           genericControllerStr :: GodotString <- toLowLevel "generic_controller"
  --           ret' <- G.call msh loadModelStr [toVariant genericControllerStr]
  --             >>= fromGodotVariant
  --           if ret'
  --             then return $ Just $ safeCast msh
  --             else return Nothing


-- Because the ARVRController member method is_button_pressed returns Int, not Bool
isButtonPressed :: Int -> GodotSimulaController -> IO Bool
isButtonPressed btnId gsc = undefined
  -- ctId <- G.get_joystick_id $ (safeCast gsc :: GodotARVRController)
  -- getSingleton GodotInput "Input" >>= \inp -> G.is_joy_button_pressed inp ctId btnId


-- | Get the window pointed at if any.
pointerWindow :: GodotSimulaController -> IO (Maybe GodotSimulaViewSprite)
pointerWindow gsc = undefined
  -- isColliding <- G.is_colliding $ _gscRayCast gsc
  -- if isColliding
  --   then G.get_collider (_gscRayCast gsc) >>= tryObjectCast @GodotSimulaViewSprite
  --   else return Nothing


-- | Change the scale of the grabbed object
rescale :: GodotSimulaController -> Float -> IO ()
rescale ct delta = undefined
 --  curPos <- V2 <$> (ct `G.get_joystick_axis` 0) <*> (ct `G.get_joystick_axis` 1)

 --  _tkBody <$> (readTVarIO (_gscTelekinesis ct)) >>= \case
 --    Just (obj, _) -> do
 --      isGripPressed <- isButtonPressed 2 ct
 --      lastPos <- readTVarIO (_gscLastScrollPos ct)
 --      curScale <- G.get_scale (safeCast obj :: GodotSpatial) >>= fromLowLevel

 --      let diff = curPos - lastPos
 --          validChange = norm lastPos > 0.01 && norm curPos > 0.01
 --          minScale = 1
 --          maxScale = 8

 --      if
 --        | norm curScale < minScale     -> rescaleBy (V2 delta delta) obj
 --        | norm curScale > maxScale     -> rescaleBy (V2 (-delta) (-delta)) obj
 --        | isGripPressed && validChange -> rescaleBy diff obj
 --        | otherwise                    -> return ()

 --    Nothing -> return ()

 --  atomically $ writeTVar (_gscLastScrollPos ct) curPos

 -- where
 --  rescaleBy :: (GodotSpatial :< child) => V2 Float -> child -> IO ()
 --  rescaleBy (V2 _ y) a =
 --    V3 1 1 1 ^* (1 + y * 0.5)
 --      & toLowLevel
 --      >>= G.scale_object_local (safeCast a :: GodotSpatial)


addSimulaController :: GodotARVROrigin -> Text -> Int -> IO GodotSimulaController
addSimulaController originNode nodeName ctID = undefined
  -- ct <- "res://addons/godot-haskell-plugin/SimulaController.gdns"
  --   & unsafeNewNS id "Object" []
  --   -- TODO: Make this implicit in newNS (godot-extra)?
  --   >>= Api.godot_nativescript_get_userdata
  --   >>= deRefStablePtr . castPtrToStablePtr

  -- G.add_child originNode (safeCast ct) True

  -- nm <- toLowLevel nodeName
  -- ct `G.set_name` nm
  -- ct `G.set_controller_id` ctID

  -- return ct


process :: GFunc GodotSimulaController
process self args = undefined
  -- delta <- getArg' 0 args :: IO Float
  -- active <- G.get_is_active self
  -- visible <- G.is_visible self

  -- if
  --   | not active -> G.set_visible self False
  --   | visible -> do
  --     rescale self delta
  --     pointerWindow self >>= \case
  --       Just window -> do
  --         G.set_visible (_gscLaser self) True
  --         pos <- G.get_collision_point $ _gscRayCast self
  --         -- processClickEvent window Motion pos
  --       Nothing -> do
  --         G.set_visible (_gscLaser self) False
  --         return ()
  --   | otherwise -> do
  --     cname <- G.get_controller_name self >>= fromLowLevel
  --     loadOpenVRControllerMesh cname >>= \case
  --       Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
  --       Nothing   -> godotPrint "Failed to set controller mesh."
  --     G.set_visible self True

  -- toLowLevel VariantNil


physicsProcess :: GFunc GodotSimulaController
physicsProcess self _ = undefined
  -- whenM (G.get_is_active self) $ do
  --   isGripPressed <- isButtonPressed 2 self
  --   triggerPull <- G.get_joystick_axis self 2
  --   let levitateCond = isGripPressed -- && triggerPull > 0.01
  --   let moveCond = triggerPull > 0.2

  --   tk <- readTVarIO (_gscTelekinesis self) >>= telekinesis levitateCond moveCond
  --   atomically $ writeTVar (_gscTelekinesis self) tk

  -- retnil
