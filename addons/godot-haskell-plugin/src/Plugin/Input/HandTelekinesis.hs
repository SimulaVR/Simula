{-# LANGUAGE LambdaCase #-}
module Plugin.Input.HandTelekinesis where

import qualified Godot.Methods                 as G
import qualified Godot.Core.GodotRigidBody     as RigidBody
import           Plugin.Imports
import           Control.Monad                            ( when )
import           Plugin.Types
import           Plugin.Input.Telekinesis

initHandTk :: (GodotSpatial :< a) => a -> IO (HandTelekinesis)
initHandTk hand = do
  -- Useful when debugging in pancake mode to modulate impulse forces:
  getSingleton Godot_Engine "Engine" >>= (`G.set_target_fps` 90)

  tf <- G.get_global_transform ((safeCast hand) :: GodotSpatial) >>= fromLowLevel
  return HandTelekinesis { _htkHand          = safeCast hand
                         , _htkBody          = Nothing
                         , _htkStrength      = 15
                         , _htkLastHandTransform = tf
                         }

handGrab :: GodotSimulaServer -> GodotRigidBody -> HandTelekinesis -> IO HandTelekinesis
handGrab gss body htk = do
  tf  <- htk & _htkHand & G.get_global_transform >>= fromLowLevel
  -- Get current values before overwriting
  cfg <- getPhysicsConfig body
  setPhysicsConfig physicsConfig body
  -- Store body with its original values
  return $ htk { _htkBody = Just (body, cfg), _htkLastHandTransform = tf }

handLetGo :: HandTelekinesis -> IO HandTelekinesis
handLetGo htk = do
  htk
    & _htkBody
    & \case
        Just (body, conf) -> do
          -- Restore original physics config
          setPhysicsConfig conf body

          return $ htk { _htkBody = Nothing }

        _ -> return htk

handManipulate :: Bool -> Float -> HandTelekinesis -> IO HandTelekinesis
handManipulate isMove factor htk = do
  tf@(TF bs pos) <- htk & _htkHand & G.get_global_transform >>= fromLowLevel
  htk
    & _htkBody
    & \case
        Just (body, _) -> do
          mass   <- G.get_mass body
          weight <- G.get_weight body

          let TF lastBs lastPos = _htkLastHandTransform htk
              power = mass * factor * factor * _htkStrength htk :: Float
              tScale = 0.001 -- Too sensitive otherwise
  
              -- Angular impulse around each axis
              tImpX = cross (lastBs ^. _z) (bs ^. _z) ^* power * tScale
              tImpY = cross (lastBs ^. _x) (bs ^. _x) ^* power * tScale
              tImpZ = cross (lastBs ^. _y) (bs ^. _y) ^* power * tScale
  
              -- Linear impulse
              lImp = power *^ (pos - lastPos) 
  
              totalImp = if isMove then norm $ tImpX + tImpY + tImpZ + lImp else 0
              rumble = 0.001 * (weight + totalImp)

          when isMove $ do
            G.apply_torque_impulse body =<< toLowLevel tImpX
            G.apply_torque_impulse body =<< toLowLevel tImpY
            G.apply_torque_impulse body =<< toLowLevel tImpZ

            p <- toLowLevel zero
            G.apply_impulse body p =<< toLowLevel lImp

          return htk { _htkLastHandTransform = tf }

        Nothing -> return htk { _htkLastHandTransform = tf }

handTryGrab :: GodotSimulaServer -> HandTelekinesis -> GodotRigidBody -> IO HandTelekinesis
handTryGrab gss htk rigidBody = htk & _htkBody & \case
          Just _  -> return htk
          Nothing -> handGrab gss rigidBody htk

handTelekinesis :: GodotSimulaServer -> HandTelekinesis -> IO HandTelekinesis
handTelekinesis gss htk = do
  let maybeRigidBody = (htk ^. htkBody)
  case maybeRigidBody of
    Just (rgb, pbc) -> handTryGrab gss htk rgb >>= handManipulate True 1
    Nothing -> do putStrLn $ "Unable to move rigid body!"
                  return htk
