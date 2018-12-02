{-# LANGUAGE TypeFamilies #-}
module Plugin.Simula (GodotSimula(..)) where

import           Plugin.Imports

import           Control.Exception

import           Foreign ( deRefStablePtr
                         , castPtrToStablePtr
                         )

import           Plugin.Input
import           Plugin.Input.Grab
import           Plugin.SimulaController
import           Plugin.WestonSurfaceSprite

import           Godot.Core.GodotGlobalConstants
import           Godot.Extra.Register
import           Godot.Nativescript
import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods as G


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
    [ GodotMethod NoRPC "_ready" ready
    , GodotMethod NoRPC "_process" process
    , GodotMethod NoRPC "on_button_signal" on_button_signal
    ]

instance HasBaseClass GodotSimula where
  type BaseClass GodotSimula = GodotNode
  super (GodotSimula obj _) = GodotNode obj


data VRInitResult
  = InitVRSuccess
  | InitVRFailed


ready :: GFunc GodotSimula
ready self _ = do
  gwc <- "res://addons/godot-haskell-plugin/WestonCompositor.gdns"
    & unsafeNewNS GodotSpatial "Spatial" []
  G.set_name gwc =<< toLowLevel "Weston"
  G.add_child self (asObj gwc) True

  initVR (safeCast self) >>= \case
    InitVRSuccess -> do
      p <- unsafeInstance GodotARVROrigin "ARVROrigin"
      G.add_child self (asObj p) True

      hmd <- unsafeInstance GodotARVRCamera "ARVRCamera"
      G.add_child self (asObj hmd) True

      newController p "LeftController"
        >>= (`G.set_controller_id` 1)
      newController p "RightController"
        >>= (`G.set_controller_id` 2)

      return ()

    InitVRFailed -> return ()

  retnil
  where
    connectController :: GodotSimulaController -> IO ()
    connectController ct = do
      btnPressed <- toLowLevel "button_pressed"
      btnReleased <- toLowLevel "button_release"
      btnSignal <- toLowLevel "on_button_signal"

      argsPressed <- Api.godot_array_new
      toLowLevel (toVariant $ asObj ct) >>= Api.godot_array_append  argsPressed
      toLowLevel (toVariant True) >>= Api.godot_array_append argsPressed

      argsReleased <- Api.godot_array_new
      toLowLevel (toVariant $ asObj ct) >>= Api.godot_array_append  argsReleased
      toLowLevel (toVariant False) >>= Api.godot_array_append argsReleased

      G.connect ct btnPressed (asObj self) btnSignal argsPressed 0
      G.connect ct btnReleased (safeCast self) btnSignal argsReleased 0

      return ()

    newController :: GodotARVROrigin -> Text -> IO GodotSimulaController
    newController p name = do
      ct <- "res://addons/godot-haskell-plugin/SimulaController.gdns"
        & unsafeNewNS id "Object" []
        -- TODO: Make this implicit in newNS (godot-extra)?
        >>= Api.godot_nativescript_get_userdata
        >>= deRefStablePtr . castPtrToStablePtr
      G.add_child p (asObj ct) True
      G.set_name ct =<< toLowLevel name
      connectController ct

      return ct


initVR :: GodotNode -> IO VRInitResult
initVR node =
  handle onVariantError $
    findInterface
      >>= G.initialize
      >>= \case
        True  -> initSuccess
        False -> initFailed
 where
  initSuccess :: IO VRInitResult
  initSuccess = do
    G.get_viewport node >>= (`G.set_use_arvr` True)
    godotPrint "Initialized OpenVR"
    return InitVRSuccess

  initFailed :: IO VRInitResult
  initFailed = do
    godotPrint "Failed to initialize OpenVR"
    return InitVRFailed

  onVariantError :: GodotVariantCallError -> IO VRInitResult
  onVariantError _ = do
    godotPrint "Failed to load OpenVR because of Variant shenanigans."
    return InitVRFailed

  findInterface :: IO GodotARVRInterface
  findInterface = do
    arvrServer <- getSingleton GodotARVRServer "ARVRServer"
    G.find_interface arvrServer =<< toLowLevel "OpenVR"


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
          >>= tryObjectCast @GodotWestonSurfaceSprite
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

