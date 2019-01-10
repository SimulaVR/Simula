{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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

import           Simula.Weston

import           Plugin.Imports
import           Plugin.WestonSurfaceSprite
import           Plugin.WestonSurfaceTexture
import           Plugin.Input.Telekinesis
import           Plugin.Pointer

import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G

import           Foreign

import Foreign.C.Types
import GHC.Float

import Debug.C
import qualified Language.C.Inline as C

initializeCppSimulaCtx

C.include "<wayland-server.h>"
C.include "<weston/weston.h>"

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
    [ GodotMethod NoRPC "_process" process
    , GodotMethod NoRPC "_physics_process" physicsProcess
    ]

instance HasBaseClass GodotSimulaController where
  type BaseClass GodotSimulaController = GodotARVRController
  super (GodotSimulaController obj _ _ _ _ _) = GodotARVRController obj


loadOpenVRControllerMesh :: Text -> IO (Maybe GodotMesh)
loadOpenVRControllerMesh name =
  "res://addons/godot-openvr/OpenVRRenderModel.gdns"
    & newNS GodotArrayMesh "ArrayMesh" [] >>= \case
      Nothing  ->
        Nothing <$ godotPrint "Couldn't find an OpenVR render model."

      Just msh -> do
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
pointerWindow :: GodotSimulaController -> IO (Maybe GodotWestonSurfaceSprite)
pointerWindow gsc = do
  isColliding <- G.is_colliding $ _gscRayCast gsc
  if isColliding
    then G.get_collider (_gscRayCast gsc) >>= tryObjectCast @GodotWestonSurfaceSprite
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
    & unsafeNewNS id "Object" []
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
      rescale self delta
      pointerWindow self >>= \case
        Just window -> do
          G.set_visible (_gscLaser self) True
          pos <- G.get_collision_point $ _gscRayCast self
          processClickEvent window Motion pos
          processTouchpadScroll self window pos
        Nothing -> do
          G.set_visible (_gscLaser self) False
          return ()
    | otherwise -> do
      cname <- G.get_controller_name self >>= fromLowLevel
      loadOpenVRControllerMesh cname >>= \case
        Just mesh -> G.set_mesh (_gscMeshInstance self) mesh
        Nothing   -> godotPrint "Failed to set controller mesh."
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

-- See http://docs.godotengine.org/en/latest/tutorials/vr/vr_starter_tutorial.html
processTouchpadScroll :: GodotSimulaController -> GodotWestonSurfaceSprite -> GodotVector3 -> IO ()
processTouchpadScroll ct gwss pos = do
  time <- getTime Realtime
  let msec = fromIntegral $ toNanoSecs time `div` 1000000
  gwst <- atomically $ readTVar $ _gwssTexture gwss
  view <- atomically $ readTVar $ _gwstView gwst
  seat <- atomically $ readTVar (_gwssSeat gwss)
  kbd <- weston_seat_get_keyboard seat

  -- Here we use inline-C as a debugging tool to print the result of
  -- `wl_list_length(weston_pointer->focus_client->pointer_resources)`.
  -- This is intended to let me see if the weston_pointer is somehow accumulating
  -- resources.
  pointer@(WestonPointer ptrToWestonPointer) <- weston_seat_get_pointer seat
  let castedPointer = (castPtr ptrToWestonPointer) :: Ptr C'WestonPointer
  numPointerResources <- [C.block| int {
                          weston_pointer *w_pointer;
                          wl_list * l_ptr;

                          w_pointer = $(weston_pointer* castedPointer);
                          l_ptr = &(w_pointer->focus_client->pointer_resources);

                          return wl_list_length(l_ptr);
                        } |]
  print $ "wl_list_length(weston_pointer->focus_client->pointer_resources): " ++ (show numPointerResources)

  curPos <- V2 <$> (ct `G.get_joystick_axis` 0) <*> (ct `G.get_joystick_axis` 1)
  lastPos <- readTVarIO (_gscLastScrollPos ct)
  let diff = curPos - lastPos
  let validChange = norm lastPos > 0.01 && norm curPos > 0.01
  let xDiff = diff ^. _x
  let yDiff = diff ^. _y

  -- if
  --   | norm curScale < minScale     -> rescaleBy (V2 delta delta) obj
  --   | norm curScale > maxScale     -> rescaleBy (V2 (-delta) (-delta)) obj
  --   | isGripPressed && validChange -> rescaleBy diff obj
  --   | otherwise                    -> return ()

  -- Make sure this doesn't interview with grab state.

  -- TODO: Change G.get_joystick_axis <0/1> to diff.<x/y>
  -- Axis 1 corresponds to the x-axis while axis 0 corresponds with the y-axis ?
  valueX <- G.get_joystick_axis ct 1
  valueY <- G.get_joystick_axis ct 0
  let valueX' = CDouble (float2Double valueX)
  let valueY' = CDouble (float2Double valueY)
  if validChange then do print $ "valueX': " ++ (show valueX')
                         print $ "valueY': " ++ (show valueY')
                 else return ()

  -- TODO: Implement something like `setPointerFocus gwc gwss timeSpec` or call
  -- weston_pointer_set_focus view sx sy (will need way to get way to retrieve sx & sy arguments; see processClickEvent)
  sendContinuousScroll pointer msec valueX' valueY'
  -- sendDiscreteScroll pointer msec valueX' valueY'

  -- TODO: Ensure another function is doing this:
  -- atomically $ writeTVar (_gscLastScrollPos ct) curPos

  -- Below we call weston_pointer_send_axis_source before sending the scroll event. Documentation suggests that this is optional for continuous events, but we call it nonetheless.
  -- Note that: wl_pointer_axis_source { WL_POINTER_AXIS_SOURCE_WHEEL = 0, WL_POINTER_AXIS_SOURCE_FINGER = 1, WL_POINTER_AXIS_SOURCE_CONTINUOUS = 2, WL_POINTER_AXIS_SOURCE_WHEEL_TILT = 3 }
  where sendContinuousScroll pointer msec valueX' valueY' = do weston_pointer_send_axis_source pointer 2 -- WL_POINTER_AXIS_SOURCE_CONTINUOUS = 2
                                                               let westonPointerAxisEventX = WestonPointerAxisEvent {
                                                                                             axis = 1 -- WL_POINTER_AXIS_HORIZONTAL_SCROLL = 1
                                                                                           , value = valueX' -- denotes length of vector in surface-local coordinate space
                                                                                           , has_discrete = False -- TODO: Experiment with sending a discrete event
                                                                                           , discrete = 0
                                                                                           }
                                                               let westonPointerAxisEventY = WestonPointerAxisEvent {
                                                                                             axis = 0 -- WL_POINTER_AXIS_VERTICAL_SCROLL = 0
                                                                                           , value = valueY' -- denotes length of vector in surface-local coordinate space
                                                                                           , has_discrete = False
                                                                                           , discrete = 0
                                                                                           }
                                                               ptrWestonPointerAxisEventX <- new westonPointerAxisEventX
                                                               ptrWestonPointerAxisEventY <- new westonPointerAxisEventY
                                                               -- Ultimately wraps wl_pointer_send_axis
                                                               weston_pointer_send_axis pointer msec ptrWestonPointerAxisEventX
                                                               weston_pointer_send_axis pointer msec ptrWestonPointerAxisEventY
                                                               free ptrWestonPointerAxisEventX
                                                               free ptrWestonPointerAxisEventY

        sendDiscreteScroll   pointer msec valueX' valueY' = do weston_pointer_send_axis_source pointer 0 -- WL_POINTER_AXIS_SOURCE_WHEEL = 0
                                                               let westonPointerAxisEventX = WestonPointerAxisEvent {
                                                                                             axis = 1 -- WL_POINTER_AXIS_HORIZONTAL_SCROLL = 1
                                                                                           , value = valueX' -- denotes length of vector in surface-local coordinate space
                                                                                           , has_discrete = True -- TODO: Experiment with sending a discrete event
                                                                                           , discrete = 1 -- test value
                                                                                           }
                                                               let westonPointerAxisEventY = WestonPointerAxisEvent {
                                                                                             axis = 0 -- WL_POINTER_AXIS_VERTICAL_SCROLL = 0
                                                                                           , value = valueY' -- denotes length of vector in surface-local coordinate space
                                                                                           , has_discrete = True
                                                                                           , discrete = 1 -- test value
                                                                                           }
                                                               ptrWestonPointerAxisEventX <- new westonPointerAxisEventX
                                                               ptrWestonPointerAxisEventY <- new westonPointerAxisEventY
                                                               -- Ultimately wraps wl_pointer_send_axis
                                                               weston_pointer_send_axis pointer msec ptrWestonPointerAxisEventX
                                                               weston_pointer_send_axis pointer msec ptrWestonPointerAxisEventY
                                                               free ptrWestonPointerAxisEventX
                                                               free ptrWestonPointerAxisEventY