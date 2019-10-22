{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell #-}

module Plugin.SimulaController
  ( GodotSimulaController(..)
  , addSimulaController
  , isButtonPressed
  , pointerWindow
  )
where

import           Control.Concurrent.STM.TVar
import           Control.Lens hiding (Context)
import           Data.Maybe

import qualified Data.Text                     as T
import           Linear

import           Plugin.Imports
import           Plugin.Types
import           Plugin.SimulaViewSprite
import           Plugin.SimulaServer
import           Plugin.Input.Telekinesis
import           Plugin.Pointer

import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Foreign ( deRefStablePtr
                         , castPtrToStablePtr
                         )
import Foreign.C.Types
import Foreign.Ptr
import GHC.Float

import           System.IO.Unsafe
import           Data.Coerce
import           Foreign
import           Foreign.C

import Godot.Api.Auto

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotMeshInstance
  , _gscTelekinesis :: TVar Telekinesis
  , _gscCurrentPos :: TVar (V2 Float)
  , _gscLastScrollPos :: TVar (V2 Float)
  , _gscDiff :: TVar (V2 Float)
  }

makeLenses ''GodotSimulaController

instance Eq GodotSimulaController where
  (==) = (==) `on` _gscObj

instance NativeScript GodotSimulaController where
  -- className = "SimulaController"
  classInit arvrController = do
    rc <- unsafeInstance GodotRayCast "RayCast"
    G.set_cast_to rc =<< toLowLevel (V3 0 0 (negate 10))
    G.set_enabled rc True
    -- G.add_child (GodotNode obj) (safeCast rc) True
    -- G.add_child (safeCast arvrController) (safeCast rc) True
    addChild arvrController rc

    ctMesh <- unsafeInstance GodotMeshInstance "MeshInstance"
    G.set_skeleton_path ctMesh =<< toLowLevel ".."
    -- G.add_child (GodotNode obj) (safeCast ctMesh) True
    -- G.add_child (safeCast arvrController) (safeCast ctMesh) True
    addChild arvrController ctMesh

    laser <- defaultPointer
    -- G.add_child (GodotNode obj) (safeCast laser) True
    -- G.add_child (safeCast arvrController) (safeCast laser) True
    addChild arvrController laser

    let tf = TF (identity :: M33 Float) (V3 0 0 0)
    tk <- newTVarIO $ initTk (GodotSpatial (safeCast arvrController)) rc tf

    lsp <- newTVarIO 0
    diff <- newTVarIO 0
    curPos <- newTVarIO 0

    G.set_visible (GodotSpatial (safeCast arvrController)) False

    return $ GodotSimulaController
      { _gscObj           = (safeCast arvrController) 
      , _gscRayCast       = rc
      , _gscMeshInstance  = ctMesh
      , _gscLaser         = laser
      , _gscTelekinesis   = tk
      , _gscLastScrollPos = lsp
      , _gscDiff          = diff
      , _gscCurrentPos    = curPos
      }

  -- classExtends = "ARVRController"
  classMethods =
    [ func NoRPC "_process" Plugin.SimulaController.process
    , func NoRPC "_physics_process" Plugin.SimulaController.physicsProcess
    ]
  classSignals = []

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController
  super (GodotSimulaController obj _ _ _ _ _ _ _) = GodotARVRController obj

loadOpenVRControllerMesh :: Text -> IO (Maybe GodotMesh)
loadOpenVRControllerMesh name = do
  -- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
  --   & newNS GodotArrayMesh "ArrayMesh" [] >>= \case

  msh <- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
    & newNS'' GodotArrayMesh "ArrayMesh" []

  loadModelStr <- toLowLevel "load_model"
  nameStr :: GodotString <- toLowLevel $ T.dropEnd 2 name
  ret <- G.call msh loadModelStr [toVariant nameStr] >>= fromGodotVariant
  if ret
    then do return $ Just $ safeCast msh
    else do genericControllerStr :: GodotString <- toLowLevel "generic_controller"
            ret' <- G.call msh loadModelStr [toVariant genericControllerStr]
              >>= fromGodotVariant
            if ret'
              then do return $ Just $ safeCast msh
              else do return Nothing

-- Because the ARVRController member method is_button_pressed returns Int, not Bool
isButtonPressed :: Int -> GodotSimulaController -> IO Bool
isButtonPressed btnId gsc = do
  -- putStrLn "isButtonPressed"
  ctId <- G.get_joystick_id $ (safeCast gsc :: GodotARVRController)
  getSingleton GodotInput "Input" >>= \inp -> G.is_joy_button_pressed inp ctId btnId


-- | Get the window pointed at if any.
pointerWindow :: GodotSimulaController -> IO (Maybe GodotSimulaViewSprite)
pointerWindow gsc = do
  -- putStrLn "pointerWindow"
  isColliding <- G.is_colliding $ _gscRayCast gsc
  if isColliding
    then G.get_collider (_gscRayCast gsc) >>= asNativeScript -- tryObjectCast @GodotSimulaViewSprite
    else return Nothing

updateTouchpadState :: GodotSimulaController -> IO ()
updateTouchpadState gsc = do
  -- oldLastPos <- readTVarIO (_gscLastScrollPos gsc)
  oldCurPos <- readTVarIO (_gscCurrentPos gsc)
  newCurPos <- V2 <$> (gsc `G.get_joystick_axis` 0) <*> (gsc `G.get_joystick_axis` 1)
  let newDiff = newCurPos - oldCurPos

  atomically $ writeTVar (_gscCurrentPos gsc) newCurPos
  atomically $ writeTVar (_gscLastScrollPos gsc) oldCurPos
  atomically $ writeTVar (_gscDiff gsc) newDiff

-- | Change the scale of the grabbed object
rescaleOrScroll :: GodotSimulaController -> Float -> IO ()
rescaleOrScroll ct delta = do
  curPos <- readTVarIO (_gscCurrentPos ct)
  lastPos <- readTVarIO (_gscLastScrollPos ct) -- Going to be same as curPos..
  diff <- readTVarIO (_gscDiff ct)
  let validChange = norm lastPos > 0.01 && norm curPos > 0.01
  isGripPressed <- isButtonPressed 2 ct

  -- This branch seems to only gets activated if we are "gripped"
  _tkBody <$> (readTVarIO (_gscTelekinesis ct)) >>= \case
    Just (obj, _) -> do
      -- isGripPressed <- isButtonPressed 2 ct
      -- lastPos <- readTVarIO (_gscLastScrollPos ct)
      curScale <- G.get_scale (safeCast obj :: GodotSpatial) >>= fromLowLevel

      let minScale = 1
          maxScale = 8

      if
        | norm curScale < minScale     -> rescaleBy (V2 delta delta) obj
        | norm curScale > maxScale     -> rescaleBy (V2 (-delta) (-delta)) obj
        | isGripPressed && validChange -> rescaleBy diff obj
        | otherwise                    -> return ()

    Nothing -> if ((not isGripPressed) && validChange)
      then scrollWindow diff
      else return ()

 where
  scrollWindow :: V2 (Float) -> IO ()
  scrollWindow diff = do
    maybeWindow <- pointerWindow ct
    wlrSeat <- getWlrSeatFromPath ct
    case maybeWindow of
      Nothing -> putStrLn "Couldn't get wlrSeat!"
      _ -> G.pointer_notify_axis_continuous wlrSeat (diff ^. _x) (diff ^. _y)
  rescaleBy :: (GodotSpatial :< child) => V2 Float -> child -> IO ()
  rescaleBy (V2 _ y) a = do
    -- maybeWindow <- pointerWindow ct
    -- case maybeWindow of
    --   Nothing -> putStrLn "Couldn't get a window!"
    --   _ -> putStrLn $ "Rescaling window!"
    V3 1 1 1 ^* (1 + y * 0.5)
      & toLowLevel
      >>= G.scale_object_local (safeCast a :: GodotSpatial)

addSimulaController :: GodotARVROrigin -> Text -> Int -> IO GodotSimulaController
addSimulaController originNode nodeName ctID = do
  -- putStrLn "addSimulaController"
  -- Requires too "large" of a type constructor:
  -- ct <- "res://addons/godot-haskell-plugin/SimulaController.gdns"
  --       & newNS'' GodotSimulaController "SimulaController" []

  -- Casts type properly; passes putStrLn inspection test:
  ct <- "res://addons/godot-haskell-plugin/SimulaController.gdns"
        & newNS'' id "Object" []
        >>= Api.godot_nativescript_get_userdata
        >>= deRefStablePtr . castPtrToStablePtr

  G.add_child originNode (safeCast ct) True

  nm <- toLowLevel nodeName
  ct `G.set_name` nm
  ct `G.set_controller_id` ctID

  return ct

process :: GodotSimulaController -> [GodotVariant] -> IO ()
process self [deltaGV] = do
  delta <- fromGodotVariant deltaGV :: IO Float
  active <- G.get_is_active self
  visible <- G.is_visible self

  if
    | not active -> G.set_visible self False
    | visible -> do
      updateTouchpadState self
      rescaleOrScroll self delta -- <- Updates SimulaController state
      pointerWindow self >>= \case
        Just window -> do
          G.set_visible (_gscLaser self) True
          pos <- G.get_collision_point $ _gscRayCast self
          processClickEvent window Motion pos
          --processTouchpadScroll self window pos
        Nothing -> do
          -- If we aren't pointing at anything, clear the wlroots seat pointer focus.
          -- TODO: See what happens if we omit this; might not need it.
          -- wlrSeat <- getWlrSeatFromPath self
          -- G.pointer_clear_focus wlrSeat -- pointer_clear_focus :: GodotWlrSeat -> IO ()

          G.set_visible (_gscLaser self) False
          return ()
    | otherwise -> do
      cname <- G.get_controller_name self >>= fromLowLevel
      loadOpenVRControllerMesh cname >>= \case
        Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
        Nothing   -> godotPrint "Failed to set controller mesh."
      G.set_visible self True

  return ()

getWlrSeatFromPath :: GodotSimulaController -> IO GodotWlrSeat
getWlrSeatFromPath self = do
  -- putStrLn "getWlrSeatFromPath"
  let nodePathStr = "/root/Root/SimulaServer" -- I'm not 100% sure this is correct!
  nodePath <- (toLowLevel (pack nodePathStr))
  gssNode  <- G.get_node ((safeCast self) :: GodotNode) nodePath
  maybeGSS      <- (asNativeScript (safeCast gssNode)) :: IO (Maybe GodotSimulaServer)
  let gss = Data.Maybe.fromJust maybeGSS
  wlrSeat  <- readTVarIO (gss ^. gssWlrSeat)

  return wlrSeat

physicsProcess :: GodotSimulaController -> [GodotVariant] -> IO ()
physicsProcess self _ = do
  whenM (G.get_is_active self) $ do
    isGripPressed <- isButtonPressed 2 self
    triggerPull <- G.get_joystick_axis self 2
    let levitateCond = isGripPressed -- && triggerPull > 0.01
    let moveCond = triggerPull > 0.2

    tk <- readTVarIO (_gscTelekinesis self) >>= telekinesis levitateCond moveCond
    atomically $ writeTVar (_gscTelekinesis self) tk

  return ()