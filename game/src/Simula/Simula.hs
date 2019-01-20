{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Simula.Simula (GodotSimula(..)) where

import           Simula.Imports
import           Telemetry

import           Simula.Input
import           Simula.Input.Grab
import           Simula.SimulaController
import           Simula.VR

import           Godot.Core.GodotGlobalConstants
import           Godot.Extra.Register
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G
import           Godot.Nativescript


data GodotSimula = GodotSimula
  { _sObj      :: GodotObject
  , _sGrabState :: TVar GrabState
  }

instance GodotClass GodotSimula where
  godotClassName = "Simula"

instance ClassExport GodotSimula where
  classInit obj  = GodotSimula obj
    <$> newTVarIO NoGrab

  classExtends = "Node"
  classMethods =
    [ func NoRPC "_ready" [] $
        \self _ -> do
          addCompositorNode self
            >>= telemetryOnConsent

          -- OpenHMD is unfortunately not yet a working substitute for OpenVR
          -- https://github.com/SimulaVR/Simula/issues/72
          openVR >>= initVR (safeCast self) >>= \case
            InitVRSuccess -> addVRNodes self
            InitVRFailed  -> return ()

    , func NoRPC "_process" ["float"] $
        \self _ ->
          let gst = _sGrabState self
          in atomically (readTVar gst)
             >>= handleState
             >>= atomically . writeTVar gst

    , func NoRPC "_on_button_signal" ["int", "SimulaController", "bool"] $
        \self [buttonVar, controllerVar, pressedVar] -> do
          button          <- fromGodotVariant buttonVar
          controllerObj   <- fromGodotVariant controllerVar
          Just controller <- tryObjectCast controllerObj
          pressed         <- fromGodotVariant pressedVar
          onButton self controller button pressed
    ] {-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

instance HasBaseClass GodotSimula where
  type BaseClass GodotSimula = GodotNode
  super (GodotSimula obj _) = GodotNode obj

addVRNodes :: GodotSimula -> IO ()
addVRNodes self = do
  -- Add the VR origin node
  orig <- unsafeInstance GodotARVROrigin "ARVROrigin"
  G.add_child self (safeCast orig) True

  -- Add the HMD as a child of the origin node
  hmd <- unsafeInstance GodotARVRCamera "ARVRCamera"
  G.add_child orig (safeCast hmd) True

  -- Add two controllers and connect their button presses to the Simula
  -- node.
  let addCt = addSimulaController orig
  addCt "LeftController" 1 >>= connectController self
  addCt "RightController" 2 >>= connectController self


addCompositorNode :: GodotSimula -> IO GodotNode
addCompositorNode self = do
  gwc <- unsafeNewNS [] "res://addons/godot-wayland/WestonCompositor.gdns"
    >>= asClass' GodotNode "Node"
  G.set_name gwc =<< toLowLevel "Weston"
  G.add_child self (asObj gwc) True

  _ <- gwc & call "use_sprites" [toVariant True]

  return gwc

-- TODO: Implement asking for user consent
telemetryOnConsent :: GodotNode -> IO ()
telemetryOnConsent compositor =
  getTelemetryConsent >>= \case
    Nothing    -> askConsent >> telemetryOnConsent compositor
    Just False -> return ()
    Just True  ->
      startTelemetry $ Telemetry $ do
        surfaceTextureArray <-
          compositor & call "get_surface_textures" []
          >>= fromGodotVariant
          :: IO GodotArray
        length <$> fromLowLevel surfaceTextureArray
 where
  askConsent = do
    putStrLn "Asking user to consent to telemetry is not yet implemented."
    putStrLn "Telemetry is disabled."
    setTelemetryConsent False

connectController :: GodotSimula -> GodotSimulaController -> IO ()
connectController self ct = do
  argsPressed <- Api.godot_array_new
  Api.godot_array_append argsPressed =<< toLowLevel (toVariant $ asObj ct)
  Api.godot_array_append argsPressed =<< toLowLevel (toVariant True)

  argsReleased <- Api.godot_array_new
  Api.godot_array_append argsReleased =<< toLowLevel (toVariant $ asObj ct)
  Api.godot_array_append argsReleased =<< toLowLevel (toVariant False)

  btnSignal   <- toLowLevel "on_button_signal"
  btnPressed  <- toLowLevel "button_pressed"
  btnReleased <- toLowLevel "button_release"

  G.connect ct btnPressed (safeCast self) btnSignal argsPressed 0
  G.connect ct btnReleased (safeCast self) btnSignal argsReleased 0

  return ()


onButton :: GodotSimula -> GodotSimulaController -> Int -> Bool -> IO ()
onButton self gsc button pressed =
  case (button, pressed) of
    (OVR_Button_Grip, False) -> -- Release grabbed
      readTVarIO gst
        >>= processGrabEvent gsc Nothing pressed
        >>= atomically
        .   writeTVar gst

    _ -> do
      let rc = _gscRayCast gsc
      pointerWindow rc
        >>= maybe (return ()) (onSpriteInput rc)
 where
  gst = _sGrabState self
  onSpriteInput rc sprite =
    G.get_collision_point rc >>= case button of
      OVR_Button_Trigger -> flip (pointerEvent (Click pressed BUTTON_LEFT)) sprite
      OVR_Button_AppMenu -> flip (pointerEvent (Click pressed BUTTON_RIGHT)) sprite
      OVR_Button_Grip    -> const $
        readTVarIO gst
          >>= processGrabEvent gsc (Just sprite) pressed
          >>= atomically
          .   writeTVar gst
      _                  -> const $ return ()

