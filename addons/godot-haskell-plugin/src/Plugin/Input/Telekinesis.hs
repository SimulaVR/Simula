{-# LANGUAGE LambdaCase #-}
module Plugin.Input.Telekinesis where

import qualified Godot.Methods                 as G
import qualified Godot.Core.GodotRigidBody     as RigidBody
import           Plugin.Imports
import           Control.Monad                            ( when )
import           Plugin.Types

data Telekinesis = Telekinesis
  { _tkController    :: GodotSpatial
  -- ^ The spatial node this is attached to
  , _tkBody          :: Maybe (GodotRigidBody, PhysicsBodyConfig)
  -- ^ List of currently manipulated bodies and their original config
  , _tkRayCast       :: GodotRayCast
  -- ^ The raycast used for detecting manipulable bodies
  , _tkRange         :: Float
  -- ^ How far we can reach
  , _tkStrength      :: Float
  -- ^ The factor for the amount of force applied
  , _tkRumble        :: Float
  -- ^ Controller rumble for some tactile feedback
  , _tkLastTransform :: Transform
  -- ^ Last known transform of the controller, used to calculate controller motion
  }

data PhysicsBodyConfig = PhysicsBodyConfig
  { _pbcGravityScale :: Float
  , _pbcLinearDamp   :: Float
  , _pbcAngularDamp  :: Float
  , _pbcMode         :: Int
  }

initTk :: (GodotSpatial :< a) => a -> GodotRayCast -> Transform -> Telekinesis
initTk ct rc tf = Telekinesis
  { _tkController    = safeCast ct
  , _tkBody          = Nothing
  , _tkRayCast       = rc
  , _tkRange         = 15
  , _tkStrength      = 15
  , _tkRumble        = 0.0
  , _tkLastTransform = tf
  }

physicsConfig :: PhysicsBodyConfig
physicsConfig = PhysicsBodyConfig
  { _pbcGravityScale = 0.0
  , _pbcLinearDamp   = 0.7
  , _pbcAngularDamp  = 0.7
  , _pbcMode         = RigidBody.MODE_RIGID
  }

getPhysicsConfig :: GodotRigidBody -> IO PhysicsBodyConfig
getPhysicsConfig body =
  PhysicsBodyConfig
    <$> G.get_gravity_scale body
    <*> G.get_linear_damp body
    <*> G.get_angular_damp body
    <*> G.get_mode body

setPhysicsConfig :: PhysicsBodyConfig -> GodotRigidBody -> IO ()
setPhysicsConfig config body = do
  body `G.set_gravity_scale` (_pbcGravityScale config)
  body `G.set_linear_damp` (_pbcLinearDamp config)
  body `G.set_angular_damp` (_pbcAngularDamp config)
  body `G.set_mode` (_pbcMode config)


grab :: GodotRigidBody -> Telekinesis -> IO Telekinesis
grab body tk = do
  tf  <- tk & _tkController & G.get_global_transform >>= fromLowLevel
  -- Get current values before overwriting
  cfg <- getPhysicsConfig body
  setPhysicsConfig physicsConfig body
  -- Disable ray so we don't pick up any new objects
  G.set_enabled (_tkRayCast tk) False
  -- Store body with its original values
  return $ tk { _tkBody = Just (body, cfg), _tkLastTransform = tf }


letGo :: Telekinesis -> IO Telekinesis
letGo tk = do
  tk
    & _tkBody
    & \case
        Just (body, conf) -> do
          -- Restore original physics config
          setPhysicsConfig conf body

          G.set_enabled (_tkRayCast tk) True

          return $ tk { _tkBody = Nothing, _tkRumble = 0 }

        _ -> return tk


manipulate :: Bool -> Float -> Telekinesis -> IO Telekinesis
manipulate isMove factor tk = do
  tf@(TF bs pos) <- tk & _tkController & G.get_global_transform >>= fromLowLevel
  tk
    & _tkBody
    & \case
        Just (body, _) -> do
          mass   <- G.get_mass body
          weight <- G.get_weight body

          let
            TF lastBs lastPos = _tkLastTransform tk
            power = mass * factor * factor * _tkStrength tk :: Float
            tScale = 0.01 -- Too sensitive otherwise

            -- Angular impulse around each axis
            tImpX = cross (lastBs ^. _z) (bs ^. _z) ^* power * tScale
            tImpY = cross (lastBs ^. _x) (bs ^. _x) ^* power * tScale
            tImpZ = cross (lastBs ^. _y) (bs ^. _y) ^* power * tScale

            -- Linear impulse
            lImp = power *^ (pos - lastPos)

            totalImp =
              if isMove then norm $ tImpX + tImpY + tImpZ + lImp else 0
            rumble = 0.001 * (weight + totalImp)

          when isMove $ do
            G.apply_torque_impulse body =<< toLowLevel tImpX
            G.apply_torque_impulse body =<< toLowLevel tImpY
            G.apply_torque_impulse body =<< toLowLevel tImpZ

            p <- toLowLevel zero
            G.apply_impulse body p =<< toLowLevel lImp

          return tk { _tkLastTransform = tf, _tkRumble = rumble }

        Nothing -> return tk { _tkLastTransform = tf, _tkRumble = 0 }


telekinesis :: Bool -> Bool -> Telekinesis -> IO Telekinesis
telekinesis isLev isMove tk = do
  _tkController tk
    &   asClass GodotARVRController "ARVRController"
    >>= \case
          Just ct -> do
            isActive    <- G.get_is_active ct
            tk' <- if isLev
              then tryGrab (_tkRayCast tk) tk >>= manipulate isMove 1
              else letGo tk

            {-G.set_rumble ct (_tkRumble tk')-}
            return tk'

          Nothing -> return tk
 where
  tryGrab rc tk' = tk' & _tkBody & \case
    Just _  -> return tk'
    Nothing -> do
      G.force_raycast_update rc
      isColliding <- (&&) <$> G.is_enabled rc <*> G.is_colliding rc
      if isColliding
        then G.get_collider rc >>= asClass GodotRigidBody "RigidBody" >>= maybe
          (return tk')
          (flip grab tk')
        else return tk'
  