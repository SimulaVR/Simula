{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}

module Plugin.LeapMotion where

import Control.Exception

import Data.Colour
import Data.Colour.SRGB.Linear

import Control.Monad
import Data.Coerce
import Unsafe.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import           Godot.Nativescript
import qualified Godot.Methods               as G
import qualified Godot.Gdnative.Internal.Api as Api

import Plugin.Types
import Plugin.SimulaViewSprite
import Data.Maybe
import Data.Either

import           Foreign
import           Foreign.Ptr
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import qualified Language.C.Inline as C

import           Control.Lens                hiding (Context)

import Data.Typeable

import qualified Data.Map.Strict as M
import Plugin.Input.HandTelekinesis

instance NativeScript GodotLeapMotion where
  className = "LeapMotion"
  classInit node = do
    do GodotLeapMotion (safeCast node)
                 <$> atomically (newTVar (error "Failed to initialize GodotLeapMotion"))
                 <*> atomically (newTVar NonIntersected)
                 <*> atomically (newTVar NonIntersected)
                 <*> atomically (newTVar 0)
                 <*> atomically (newTVar Nothing)
                 <*> atomically (newTVar Nothing)
                 <*> atomically (newTVar Nothing)
                 <*> atomically (newTVar 0)
  classMethods =
    [
      func NoRPC "_ready" (catchGodot Plugin.LeapMotion._ready)
    , func NoRPC "_process" (catchGodot Plugin.LeapMotion._process)
    , func NoRPC "_physics_process" (catchGodot Plugin.LeapMotion._physics_process)
    , func NoRPC "_new_hand" (catchGodot Plugin.LeapMotion._new_hand)
    , func NoRPC "_hand_intersect_left" (catchGodot Plugin.LeapMotion._hand_intersect_left)
    , func NoRPC "_hand_intersect_right" (catchGodot Plugin.LeapMotion._hand_intersect_right)
    , func NoRPC "_hand_exit_left" (catchGodot Plugin.LeapMotion._hand_exit_left)
    , func NoRPC "_hand_exit_right" (catchGodot Plugin.LeapMotion._hand_exit_right)
    , func NoRPC "_about_to_remove_hand" (catchGodot Plugin.LeapMotion._about_to_remove_hand)
    ]

_ready :: GodotLeapMotion -> [GodotVariant] -> IO ()
_ready glm args = do
  putStrLn $ "GodotLeapMotion _ready"
  gss <- readTVarIO (glm ^. glmServer)
  putStrLn $ "GodotLeapMotion _ready"
  gss <- readTVarIO (glm ^. glmServer)

  connectGodotSignal glm "new_hand" glm "_new_hand" [] -- emmitted from gldm_sensor.cpp
  connectGodotSignal glm "about_to_remove_hand" glm "_about_to_remove_hand" [] -- emmitted from gldm_sensor.cpp

  -- Enable visible collision regions for easier debugging
  putStrLn $ "Engaging Godot debug collisions.."
  sceneTree <- G.get_tree gss -- getSingleton GodotSceneTree "SceneTree"
  G.set_debug_collisions_hint sceneTree True

  -- Push the leap tree back a bit so that hands spawn already intersected with
  -- the center app (allowing for easier debugging)
  tVec <- toLowLevel $ V3 0 0 (-1.25)
  G.set_translation glm tVec

  return ()

_process :: GodotLeapMotion -> [GodotVariant] -> IO ()
_process gdlm args = do
  return ()

getHandData :: GodotLeapMotion -> HandSide -> IO (Maybe (HandState, Bool, Bool, GodotSpatial))
getHandData glm LeftHand = do
  maybeHandLeft <- readTVarIO (glm ^. glmLeftHand)
  case maybeHandLeft of
    Nothing -> return Nothing
    Just handLeft -> do leftHandState <- readTVarIO (glm ^. glmLeftHandState)
                        isLeftPinched <- G.get_is_pinched_left glm
                        isLeftGrabbed <- G.get_is_grabbed_left glm
                        handSpatialLeft <- readTVarIO (handLeft ^. handSpatial)
                        return $ Just (leftHandState, isLeftPinched, isLeftGrabbed, handSpatialLeft)
getHandData glm RightHand = do
  maybeHandRight <- readTVarIO (glm ^. glmRightHand)
  case maybeHandRight of
    Nothing -> return Nothing
    Just handRight -> do rightHandState <- readTVarIO (glm ^. glmRightHandState)
                         isRightPinched <- G.get_is_pinched_right glm
                         isRightGrabbed <- G.get_is_grabbed_right glm
                         handSpatialRight <- readTVarIO (handRight ^. handSpatial)
                         return $ Just (rightHandState, isRightPinched, isRightGrabbed, handSpatialRight)

updateHandState :: GodotLeapMotion -> HandSide -> IO (Maybe HandState)
updateHandState glm handSide = do
 maybeHandState <- getHandData glm handSide
 case maybeHandState of
      Nothing -> return Nothing
      Just (handState, isPinched, isGrabbed, handSpatial) -> do
            case handState of
                  NonIntersected -> return (Just NonIntersected) -- Prevent gsvs manipulation unless user starts in `Intersected gsvs` state
                  Intersected gsvs -> case (isPinched, isGrabbed) of
                                            (True, _) -> do
                                              return $ (Just $ Pinched gsvs)
                                            (_, True) -> do -- Start grab
                                                            handTk <- initHandTk handSpatial >>= handGrab (safeCast gsvs)
                                                            return $ (Just $ Grabbed handTk)
                                            _ -> return $ (Just $ Intersected gsvs)
                  Pinched gsvs -> case (isPinched, isGrabbed) of
                                        (False, False) -> do -- Release pinch
                                                            atomically $ writeTVar (glm ^. glmPinchDist) Nothing
                                                            return $ (Just $ Intersected gsvs)
                                        (False, True) -> do -- Release pinch
                                                            atomically $ writeTVar (glm ^. glmPinchDist) Nothing
                                                            -- Shift to grab
                                                            handTk <- initHandTk handSpatial >>= handGrab (safeCast gsvs)
                                                            return $ (Just $ Grabbed handTk)
                                        (True, _) -> do -- Continue pinching
                                                        return $ (Just $ Pinched gsvs)
                  Grabbed handTk -> case (isPinched, isGrabbed) of
                                          (False, False) -> do -- Release Grab
                                                              let (gsvs', pbc) = Data.Maybe.fromJust (handTk ^. htkBody)
                                                              maybeGSVS <- asNativeScript (safeCast gsvs') :: IO (Maybe GodotSimulaViewSprite)
                                                              let gsvs = Data.Maybe.fromJust maybeGSVS
                                                              handLetGo handTk
                                                              return (Just $ Intersected ((gsvs) :: GodotSimulaViewSprite))
                                          (True, False) -> do -- Shift to pinch
                                                              let (gsvs', pbc) = Data.Maybe.fromJust (handTk ^. htkBody)
                                                              maybeGSVS <- asNativeScript ((safeCast gsvs') :: GodotObject) :: IO (Maybe GodotSimulaViewSprite)
                                                              let gsvs = Data.Maybe.fromJust maybeGSVS
                                                              handLetGo handTk
                                                              return (Just $ Pinched ((gsvs) :: GodotSimulaViewSprite))
                                          (_, True) -> do -- Continue grabbing
                                                          return $ (Just $ Grabbed handTk)


_physics_process :: GodotLeapMotion -> [GodotVariant] -> IO ()
_physics_process glm args@[deltaGV] = do
  -- Get the latest hand state for this frame
  maybeLeftHandState <- updateHandState glm LeftHand
  maybeRightHandState <- updateHandState glm RightHand

  -- Engage in pinch/grab actions, and update state
  case (maybeLeftHandState, maybeRightHandState) of
    (Nothing, Nothing) -> return ()
    (Just leftHandState, Nothing) -> do
      case leftHandState of
        NonIntersected -> return ()
        Intersected gsvs1 -> return ()
        Pinched gsvs1 -> return ()
        Grabbed handTk1 -> handTelekinesis handTk1 >> return ()
      atomically $ writeTVar (glm ^. glmLeftHandState) leftHandState
    (Nothing, Just rightHandState)-> do
      case rightHandState of
        NonIntersected -> return ()
        Intersected gsvs2 -> return ()
        Pinched gsvs2 -> return ()
        Grabbed handTk2 -> handTelekinesis handTk2 >> return ()
      atomically $ writeTVar (glm ^. glmRightHandState) rightHandState
    (Just leftHandState, Just rightHandState) -> do
      case (leftHandState, rightHandState) of
        (Pinched gsvs1, Pinched gsvs2) -> do
          twoHandScale glm gsvs1 gsvs2
          putStrLn $ "twoHandScale"
        (Grabbed handTk1, Grabbed handTk2) -> do
          handTelekinesis handTk1 >> return ()
          handTelekinesis handTk2 >> return ()
          putStrLn $ "Grab left & right"
        (Grabbed handTk1, _) -> handTelekinesis handTk1 >> (putStrLn "Grab left")
        (_, Grabbed handTk2) -> handTelekinesis handTk2 >> (putStrLn "Grab right")
        (Pinched gsvs1, _) -> putStrLn "(Pinched gsvs1, _)"
        (_, Pinched gsvs2) -> putStrLn "(_, Pinched gsvs1)"
        (Intersected gsvs1, Intersected gsvs2) -> putStrLn $ "(Intersected gsvs1, Intersected gsvs2)"
        (Intersected gsvs1, _) -> putStrLn $ "(Intersected gsvs1, _)"
        (_, Intersected gsvs2) -> putStrLn $ "(_, Intersected gsvs2)"
        _ -> putStrLn $ "Doing nothing.."
      atomically $ writeTVar (glm ^. glmLeftHandState) leftHandState
      atomically $ writeTVar (glm ^. glmRightHandState) rightHandState


_new_hand :: GodotLeapMotion -> [GodotVariant] -> IO ()
_new_hand glm args@[godotSpatialHand', handType'] = do
  putStrLn "_new_hand"
  godotSpatialHand <- fromGodotVariant godotSpatialHand' :: IO GodotSpatial
  handType <- fromGodotVariant handType' :: IO Int

  -- Instantiate state
  godotArea <- unsafeInstance GodotArea "Area"
  godotSphereShape <- unsafeInstance GodotSphereShape "SphereShape"
  G.set_radius godotSphereShape 1.0 -- TODO: Optimize the hand-push collision region

  -- glm ← Spatial
    -- addChild glm godotSpatialHand -- I think this is done in gdlm_sensor.cpp, so not necessary

  -- Spatial ← Area
  addChild godotSpatialHand godotArea

  -- Area ← Shape
  ownerId <- G.create_shape_owner godotArea (safeCast godotArea)
  G.shape_owner_add_shape godotArea ownerId (safeCast godotSphereShape)


  -- Update state
  handSpatialTVar <- newTVarIO godotSpatialHand
  handAreaTVar <- newTVarIO godotArea
  handSphereShapeTVar <- newTVarIO godotSphereShape

  let newLeapHand = LeapHand {
          _handSpatial = handSpatialTVar
        , _handArea = handAreaTVar
        , _handSphere = handSphereShapeTVar
      }

  case handType of
        0 -> do putStrLn "CONNECTING LEFT HAND SIGNALS"
                connectGodotSignal godotArea "body_shape_entered" glm "_hand_intersect_left" [] -- emmitted from gldm_sensor.cpp
                connectGodotSignal godotArea "body_shape_exited" glm "_hand_exit_left" [] -- emmitted from gldm_sensor.cpp
                atomically $ writeTVar (glm ^. glmLeftHand) (Just newLeapHand)
        1 -> do putStrLn "CONNECTING RIGHT_HAND SIGNALS"
                connectGodotSignal godotArea "body_shape_entered" glm "_hand_intersect_right" [] -- emmitted from gldm_sensor.cpp

                connectGodotSignal godotArea "body_shape_exited" glm "_hand_exit_right" [] -- emmitted from gldm_sensor.cpp
                atomically $ writeTVar (glm ^. glmRightHand) (Just newLeapHand)
        n -> putStrLn $ "handType: " ++ (show n)

  atomically $ modifyTVar' (glm ^. glmHandCount) (+1)
  hc <- readTVarIO (glm ^. glmHandCount)
  putStrLn $ "_new_hand " ++ (show hc)
  if (hc >= 2) then G.set_physics_process glm True else return ()

_hand_intersect_left :: GodotLeapMotion -> [GodotVariant] -> IO ()
_hand_intersect_left glm args@[bodyId', body', bodyShape', localShape'] = do
  putStrLn "_hand_intersect_left"
  body <- fromGodotVariant body' :: IO GodotNode
  isLeftPinched <- G.get_is_pinched_left glm
  isLeftGrabbed <- G.get_is_grabbed_left glm
  maybeGSVS <- asNativeScript ((safeCast body) :: GodotObject) :: IO (Maybe GodotSimulaViewSprite)

  case (maybeGSVS, isLeftPinched, isLeftGrabbed) of
       (Nothing, _, _) -> putStrLn "_hand_intersect with non-GSVS object"
       (_, True, _) -> do return () -- Make interaction with a gsvs while already pinched
       (_, _, True) -> do return () -- ..or gripped impossible
       (Just gsvs, False, False) -> atomically $ writeTVar (glm ^. glmLeftHandState) (Intersected gsvs)

_hand_intersect_right :: GodotLeapMotion -> [GodotVariant] -> IO ()
_hand_intersect_right glm args@[bodyId', body', bodyShape', localShape'] = do
  putStrLn "_hand_intersect_right"
  body <- fromGodotVariant body' :: IO GodotNode
  isRightPinched <- G.get_is_pinched_right glm
  isRightGrabbed <- G.get_is_grabbed_right glm
  maybeGSVS <- asNativeScript ((safeCast body) :: GodotObject) :: IO (Maybe GodotSimulaViewSprite)

  case (maybeGSVS, isRightPinched, isRightGrabbed) of
       (Nothing, _, _) -> putStrLn "_hand_intersect with non-GSVS object"
       (_, True, _) -> do return () -- Make interaction with a gsvs while already pinched
       (_, _, True) -> do return () -- ..or gripped impossible
       (Just gsvs, False, False) -> atomically $ writeTVar (glm ^. glmRightHandState) (Intersected gsvs)

_hand_exit_left :: GodotLeapMotion -> [GodotVariant] -> IO ()
_hand_exit_left glm args@[bodyId', body', bodyShape', localShape'] = do
  putStrLn $ "_hand_exit_left"
  atomically $ writeTVar (glm ^. glmRightHandState) NonIntersected

_hand_exit_right :: GodotLeapMotion -> [GodotVariant] -> IO ()
_hand_exit_right glm args@[bodyId', body', bodyShape', localShape'] = do
  putStrLn $ "_hand_exit_right"
  atomically $ writeTVar (glm ^. glmRightHandState) NonIntersected

twoHandScale :: GodotLeapMotion -> GodotSimulaViewSprite -> GodotSimulaViewSprite -> IO ()
twoHandScale glm gsvs1 gsvs2 = do
  putStrLn $ "twoHandScale"
  maybeOldDist <- readTVarIO (glm ^. glmPinchDist)

  leftHand' <- readTVarIO (glm ^. glmLeftHand) >>= (return . Data.Maybe.fromJust)
  rightHand' <- (readTVarIO (glm ^. glmRightHand)) >>= (return . Data.Maybe.fromJust)
  leftHand <- readTVarIO (leftHand' ^. handSpatial)
  rightHand <- readTVarIO (rightHand' ^. handSpatial)
  leftHandPos <- G.get_global_transform leftHand >>= Api.godot_transform_get_origin
  rightHandPos <- G.get_global_transform rightHand >>= Api.godot_transform_get_origin
  newDist <- realToFrac <$> Api.godot_vector3_distance_to leftHandPos rightHandPos
  let oldDist = case maybeOldDist of
                    Nothing -> newDist
                    Just oldDist -> oldDist
  let scale = (newDist / oldDist)
  atomically $ writeTVar (glm ^. glmPinchDist) (Just newDist) --

twoHandScaleViaLeap :: GodotLeapMotion -> GodotSimulaViewSprite -> GodotSimulaViewSprite -> IO ()
twoHandScaleViaLeap glm gsvs1 gsvs2 = do
  putStrLn $ "twoHandScaleViaLeap"
  scale <- G.get_hands_scale_factor glm
  toLowLevel (V3 scale scale scale) >>= G.scale_object_local ((safeCast gsvs1) :: GodotSpatial)

-- TODO: Clean up state here, if needed
_about_to_remove_hand :: GodotLeapMotion -> [GodotVariant] -> IO ()
_about_to_remove_hand glm args@[handSpatial'] = do
  putStrLn "_about_to_remove_hand"
  -- TODO: Bifurcate the "about_to_remove_hand" signal into a "*_left" and "*_right", to help clean up state
  -- handSpatial <- fromGodotVariant handSpatial' :: IO GodotNode
  -- leftHand <- readTVarIO (glm ^. glmLeftHand)
  -- godotNode <- readTVarIO (leftHand ^. handNode)
  -- godotArea <- readTVarIO (leftHand ^. handArea)
  -- godotSphereShape <- readTVarIO (leftHand ^. handSphere)
  -- -- G.queue_free godotNode -- done in GDLMSensor::delete_hand
  -- G.queue_free godotArea
  -- G.queue_free godotSphereShape