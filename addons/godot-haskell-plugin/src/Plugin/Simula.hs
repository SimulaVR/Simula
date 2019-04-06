{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Plugin.Simula (GodotSimula(..)) where

import           Plugin.Imports

import           Plugin.Input
import           Plugin.Input.Grab
import           Plugin.SimulaController
import           Plugin.SimulaViewSprite
import           Plugin.VR
import           Plugin.SimulaViewTexture

import           Godot.Core.GodotGlobalConstants
import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api   as Api
import qualified Godot.Methods                 as G


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
    [ GodotMethod NoRPC "_ready" Plugin.Simula.ready
    , GodotMethod NoRPC "_process" Plugin.Simula.process
    , GodotMethod NoRPC "on_button_signal" Plugin.Simula.on_button_signal
    ]
  classSignals = []

instance HasBaseClass GodotSimula where
  type BaseClass GodotSimula = GodotNode
  super (GodotSimula obj _) = GodotNode obj


ready :: GFunc GodotSimula
ready self _ = do
  addCompositorNode

  -- OpenHMD is unfortunately not yet a working substitute for OpenVR
  -- https://github.com/SimulaVR/Simula/issues/72
  openVR >>= initVR (safeCast self) >>= \case
    InitVRSuccess -> do
      -- Add the VR origin node
      orig <- unsafeInstance GodotARVROrigin "ARVROrigin"
      G.add_child self (safeCast orig) True

      -- Add the HMD as a child of the origin node
      hmd <- unsafeInstance GodotARVRCamera "ARVRCamera"
      G.add_child orig (safeCast hmd) True

      -- Add two controllers and connect their button presses to the Simula
      -- node.
      let addCt = addSimulaController orig
      addCt "LeftController" 1 >>= connectController
      addCt "RightController" 2 >>= connectController

      return ()

    InitVRFailed  -> return ()

  retnil
 where
  addCompositorNode :: IO ()
  addCompositorNode = do
    -- gwc <- "res://addons/godot-haskell-plugin/SimulaServer.gdns"
    --   & unsafeNewNS GodotSpatial "Spatial" []
    gwcObj <- newNS' [] "res://addons/godot-haskell-plugin/SimulaServer.gdns"
    gwc <- Plugin.Imports.fromNativeScript gwcObj :: IO GodotSpatial

    G.set_name gwc =<< toLowLevel "Simula"
    G.add_child self (asObj gwc) True

  connectController :: GodotSimulaController -> IO ()
  connectController ct = do
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


on_button_signal :: GFunc GodotSimula
on_button_signal self args = do
  case toList args of
    [buttonVar, controllerVar, pressedVar] -> do
      button <- fromGodotVariant buttonVar
      controllerObj <- fromGodotVariant controllerVar
      Just controller <- tryObjectCast controllerObj
      pressed <- fromGodotVariant pressedVar
      onButton self controller button pressed
    _ -> return ()
  toLowLevel VariantNil


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
      whenM (G.is_colliding rc) $
        G.get_collider rc
          >>= tryObjectCast @GodotSimulaViewSprite
          >>= maybe (return ()) (onSpriteInput rc)
 where
  gst = _sGrabState self
  onSpriteInput rc sprite =
    G.get_collision_point rc >>= case button of
      OVR_Button_Trigger -> processClickEvent sprite (Button pressed BUTTON_LEFT)
      OVR_Button_AppMenu -> processClickEvent sprite (Button pressed BUTTON_RIGHT)
      OVR_Button_Grip    -> const $
        readTVarIO gst
          >>= processGrabEvent gsc (Just sprite) pressed
          >>= atomically
          .   writeTVar gst
      _                  -> const $ return ()


process :: GFunc GodotSimula
process self _ = do
  let gst = _sGrabState self
  atomically (readTVar gst)
    >>= handleState
    >>= atomically . writeTVar gst

  toLowLevel VariantNil