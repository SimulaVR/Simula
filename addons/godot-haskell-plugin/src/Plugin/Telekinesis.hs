{-# LANGUAGE LambdaCase #-}
module Plugin.Telekinesis where

import qualified Godot.Methods as G
import qualified Godot.Core.GodotRigidBody as RigidBody
import           Plugin.Imports
import           Control.Monad (when)


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
  }

initTk :: GodotSpatial -> GodotRayCast -> Transform -> Telekinesis
initTk ct rc tf = Telekinesis
  { _tkController    = ct
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
  , _pbcLinearDamp   = 0.5
  , _pbcAngularDamp  = 0.7
  }

getPhysics :: GodotRigidBody -> IO PhysicsBodyConfig
getPhysics body =
  PhysicsBodyConfig
    <$> G.get_gravity_scale body
    <*> G.get_linear_damp body
    <*> G.get_angular_damp body

setPhysics :: PhysicsBodyConfig -> GodotRigidBody -> IO ()
setPhysics config body = do
  body `G.set_gravity_scale` (_pbcGravityScale config)
  body `G.set_linear_damp`   (_pbcLinearDamp   config)
  body `G.set_angular_damp`  (_pbcAngularDamp  config)


grab :: GodotRigidBody -> Telekinesis -> IO Telekinesis
grab body tk = do
  G.set_mode body RigidBody.MODE_RIGID
  -- Get current values before overwriting
  cfg <- getPhysics body
  setPhysics physicsConfig body
  -- Disable ray so we don't pick up any new objects
  G.set_enabled (_tkRayCast tk) False
  -- Store body with its original values
  return $ tk { _tkBody = Just (body, cfg) }


letGo :: Telekinesis -> IO Telekinesis
letGo tk = do
  tk & _tkBody & \case
    Just (body, conf) -> do
      parentName <- G.get_parent body >>= G.get_name >>= fromLowLevel
      when (parentName == "Weston") $ G.set_mode body RigidBody.MODE_KINEMATIC

      -- Restore original physics config
      setPhysics conf body

      G.set_enabled (_tkRayCast tk) True
      return $ tk { _tkBody = Nothing, _tkRumble = 0 }
    _ -> return tk

manipulate :: Bool -> Float -> Telekinesis -> IO Telekinesis
manipulate isMove factor tk = do
  tk & _tkBody & \case
    Just (body, _) -> do
      mass      <- G.get_mass body
      weight    <- G.get_weight body
      tf@(TF bs pos) <- tk & _tkController & G.get_global_transform >>= fromLowLevel

      let TF lastBs lastPos = _tkLastTransform tk
          power    = mass * factor * factor * _tkStrength tk :: Float
          tScale   = 0.01 -- Too sensitive otherwise
          tImpX    = cross (lastBs^._z) (bs^._z) ^* power * tScale
          tImpY    = cross (lastBs^._x) (bs^._x) ^* power * tScale
          tImpZ    = cross (lastBs^._y) (bs^._y) ^* power * tScale
          lImp     = power *^ (pos - lastPos)
          totalImp = if isMove then norm $ tImpX + tImpY + tImpZ + lImp else 0
          rumble   = 0.005 * (weight + totalImp)

      when isMove $ do
        -- Angular
        G.apply_torque_impulse body =<< toLowLevel tImpX
        G.apply_torque_impulse body =<< toLowLevel tImpY
        G.apply_torque_impulse body =<< toLowLevel tImpZ

        -- Linear
        p <- toLowLevel zero
        G.apply_impulse body p =<< toLowLevel lImp

      return tk { _tkLastTransform = tf, _tkRumble = rumble }

    Nothing -> return tk { _tkRumble = 0 }



telekinesis :: Bool -> Telekinesis -> IO Telekinesis
telekinesis isLev tk = do
  _tkController tk `asClass` (GodotARVRController, "ARVRController") >>= \case
    Just ct -> do
      isActive    <- G.get_is_active ct
      triggerPull <- if isActive then G.get_joystick_axis ct 2 else return 0
      tk' <-
        if isLev
        then tryGrab (_tkRayCast tk) tk >>= manipulate (triggerPull > 0.01) triggerPull
        else letGo tk

      G.set_rumble ct (_tkRumble tk')
      return tk'

    Nothing -> return tk
 where
  tryGrab rc tk' =
    tk' & _tkBody & \case
      Just _ -> return tk'
      Nothing -> do
        isColliding <- (&&) <$> G.is_enabled rc <*> G.is_colliding rc
        if isColliding
        then G.get_collider rc
            >>= flip asClass (GodotRigidBody, "RigidBody")
            >>= maybe (return tk') (flip grab tk')
        else return tk'
