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

import qualified Data.Text                     as T
import           Linear

import           Plugin.Imports
import           Plugin.Types
import           Plugin.SimulaViewSprite
import           Plugin.SimulaServer
import           Plugin.Input.Telekinesis
import           Plugin.Pointer

import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Graphics.Wayland.WlRoots.Seat
import           Graphics.Wayland.WlRoots.Input.Pointer

import           Foreign ( deRefStablePtr
                         , castPtrToStablePtr
                         )
import Foreign.C.Types
import Foreign.Ptr
import GHC.Float

data GodotSimulaController = GodotSimulaController
  { _gscObj     :: GodotObject
  , _gscRayCast :: GodotRayCast
  , _gscMeshInstance :: GodotMeshInstance
  , _gscLaser :: GodotMeshInstance
  , _gscTelekinesis :: TVar Telekinesis
  , _gscLastScrollPos :: TVar (V2 Float)
  }

makeLenses ''GodotSimulaController

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

    return $ GodotSimulaController
      { _gscObj           = obj
      , _gscRayCast       = rc
      , _gscMeshInstance  = ctMesh
      , _gscLaser         = laser
      , _gscTelekinesis   = tk
      , _gscLastScrollPos = lsp
      }

  classExtends = "ARVRController"
  classMethods =
    [ GodotMethod NoRPC "_process" Plugin.SimulaController.process
    , GodotMethod NoRPC "_physics_process" Plugin.SimulaController.physicsProcess
    ]
  classSignals = []

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController
  super (GodotSimulaController obj _ _ _ _ _) = GodotARVRController obj

loadOpenVRControllerMesh :: Text -> IO (Maybe GodotMesh)
loadOpenVRControllerMesh name = do
  -- "res://addons/godot-openvr/OpenVRRenderModel.gdns"
  --   & newNS GodotArrayMesh "ArrayMesh" [] >>= \case
  maybeObj <- newNS [] "res://addons/godot-openvr/OpenVRRenderModel.gdns" 
  case maybeObj of
      Nothing  ->
        Nothing <$ godotPrint "Couldn't find an OpenVR render model."

      Just obj -> do
        msh <- Plugin.Imports.fromNativeScript obj :: IO GodotArrayMesh
        loadModelStr <- toLowLevel "load_model"
        nameStr :: GodotString <- toLowLevel $ T.dropEnd 2 name
        ret <- G.call msh loadModelStr [toVariant nameStr] >>= fromGodotVariant
        if ret
          then return $ Just $ safeCast msh
          else do
            genericControllerStr :: GodotString <- toLowLevel "generic_controller"
            ret' <- G.call msh loadModelStr [toVariant genericControllerStr]
              >>= fromGodotVariant
            if ret'
              then return $ Just $ safeCast msh
              else return Nothing


-- Because the ARVRController member method is_button_pressed returns Int, not Bool
isButtonPressed :: Int -> GodotSimulaController -> IO Bool
isButtonPressed btnId gsc = do
  ctId <- G.get_joystick_id $ (safeCast gsc :: GodotARVRController)
  getSingleton GodotInput "Input" >>= \inp -> G.is_joy_button_pressed inp ctId btnId


-- | Get the window pointed at if any.
pointerWindow :: GodotSimulaController -> IO (Maybe GodotSimulaViewSprite)
pointerWindow gsc = do
  isColliding <- G.is_colliding $ _gscRayCast gsc
  if isColliding
    then G.get_collider (_gscRayCast gsc) >>= tryObjectCast @GodotSimulaViewSprite
    else return Nothing

-- | Change the scale of the grabbed object
rescale :: GodotSimulaController -> Float -> IO ()
rescale ct delta = do
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

      -- NOTE: Here might be a good time to call beginInteractive (if needed)
      if
        | norm curScale < minScale     -> rescaleBy (V2 delta delta) obj
        | norm curScale > maxScale     -> rescaleBy (V2 (-delta) (-delta)) obj
        | isGripPressed && validChange -> rescaleBy diff obj
        | otherwise                    -> return ()

    Nothing -> return ()

  atomically $ writeTVar (_gscLastScrollPos ct) curPos

 where
  rescaleBy :: (GodotSpatial :< child) => V2 Float -> child -> IO ()
  rescaleBy (V2 _ y) a =
    V3 1 1 1 ^* (1 + y * 0.5)
      & toLowLevel
      >>= G.scale_object_local (safeCast a :: GodotSpatial)


addSimulaController :: GodotARVROrigin -> Text -> Int -> IO GodotSimulaController
addSimulaController originNode nodeName ctID = do
  ct <- "res://addons/godot-haskell-plugin/SimulaController.gdns"
    & newNS' []
    -- TODO: Make this implicit in newNS (godot-extra)?
    >>= Api.godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr

  G.add_child originNode (safeCast ct) True

  nm <- toLowLevel nodeName
  ct `G.set_name` nm
  ct `G.set_controller_id` ctID

  return ct

process :: GFunc GodotSimulaController
process self args = do
  delta <- getArg' 0 args :: IO Float
  active <- G.get_is_active self
  visible <- G.is_visible self

  if
    | not active -> G.set_visible self False
    | visible -> do
      rescale self delta -- <- Updates SimulaController state
      pointerWindow self >>= \case
        Just window -> do
          G.set_visible (_gscLaser self) True
          pos <- G.get_collision_point $ _gscRayCast self
          processClickEvent window Motion pos
          processTouchpadScroll self window pos
        Nothing -> do
          -- If we aren't pointing at anything, clear the wlroots seat pointer focus.
          -- TODO: See what happens if we omit this; might not need it.
          wlrSeat <- getWlrSeatFromPath self
          G.pointer_clear_focus wlrSeat -- pointer_clear_focus :: GodotWlrSeat -> IO ()

          G.set_visible (_gscLaser self) False
          return ()
    | otherwise -> do
      cname <- G.get_controller_name self >>= fromLowLevel
      loadOpenVRControllerMesh cname >>= \case
        Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
        Nothing   -> godotPrint "Failed to set controller mesh."
      G.set_visible self True

  toLowLevel VariantNil
  where
    getWlrSeatFromPath :: GodotSimulaController -> IO GodotWlrSeat
    getWlrSeatFromPath self = do
      let nodePathStr = "/root/Root/SimulaServer" -- I'm not 100% sure this is correct!
      nodePath <- (toLowLevel (pack nodePathStr))
      gssNode  <- G.get_node ((safeCast self) :: GodotNode) nodePath
      gss      <- (fromNativeScript (safeCast gssNode)) :: IO GodotSimulaServer -- Recall we had trouble with this call in Simula.hs (see newNS''); it might actually work in this context, though.
      wlrSeat  <- readTVarIO (gss ^. gssWlrSeat)

      return wlrSeat

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

-- See http://docs.godotengine.org/en/latest/tutorials/vr/vr_starter_tutorial.html
-- Ultimately should wrap a call to
processTouchpadScroll :: GodotSimulaController -> GodotSimulaViewSprite -> GodotVector3 -> IO ()
processTouchpadScroll ct gsvs pos = do
  -- Get a bunch of needed state
  -- time <- getTime Realtime
  -- let msec = fromIntegral $ toNanoSecs time `div` 1000000
  -- gsvt <- atomically $ readTVar $ _gsvsTexture gsvs
  -- simulaView <- atomically $ readTVar (gsvt ^. gsvtView)
  -- let seat = (simulaView ^. svServer ^. gssSeat)
  -- curPos <- V2 <$> (ct `G.get_joystick_axis` 0) <*> (ct `G.get_joystick_axis` 1)
  -- lastPos <- readTVarIO (_gscLastScrollPos ct)
  -- let diff = curPos - lastPos
  -- let validChange = norm lastPos > 0.01 && norm curPos > 0.01
  -- let xDiff = diff ^. _x
  -- let yDiff = diff ^. _y
  -- let minScale = 1
  -- let maxScale = 8

  -- Print a debug message
  printDebugAxis

  -- Send an "axis" event (for y-axis only) to the currently focused surface,
  -- provided the scroll event is past the required magnitude.
  -- TODO: Optimize these thresholds
  -- if
  --   | xDiff < 0.0001     -> return ()
  --   | yDiff < 0.0001     -> return ()
  --   | otherwise          -> sendAxisEvent seat msec AxisVertical yDiff -- >> sendAxisEvent seat msec AxisHorizontal xDiff

  -- -- Update controller state.
  -- -- NOTE: `rescale` actually does this for us, but it's harmless/safer to
  -- --       do it again:
  -- atomically $ writeTVar (ct ^. gscLastScrollPos) curPos

  where printDebugAxis = do
          -- NOTE: Axis 1 corresponds to the x-axis while axis 0 corresponds
          -- with the y-axis (matrix style)
          valueX <- G.get_joystick_axis ct 1
          valueY <- G.get_joystick_axis ct 0
          let valueX' = CDouble (float2Double valueX)
          let valueY' = CDouble (float2Double valueY)
          print $ "valueX': " ++ (show valueX')
          print $ "valueY': " ++ (show valueY')
          
        -- | I'm (only somewhat) confident that this sends a "continuous" scroll
        -- | event to the currently focused surface (which implies we don't have
        -- | to mess with i.e. `viewAt`).
        -- sendAxisEvent seat time32 axisOrientation axisDeltaValue = do
        --   let time32          = time32
        --   let axisSource      = AxisContinuous :: AxisSource -- We don't even use this value?
        --   let axisOrientation = axisOrientation :: AxisOrientation -- data AxisOrientation = AxisVertical | AxisHorizontal
        --   let axisDeltaValue  = axisDeltaValue :: Double
        --   let axisDiscrete    = 0 -- mwheelup = -1, mwheeldown = 1; 
        --                           -- I believe "0" means "continuous"; try that first

        --   pointerNotifyAxis seat time32 axisOrientation axisDeltaValue axisDiscrete