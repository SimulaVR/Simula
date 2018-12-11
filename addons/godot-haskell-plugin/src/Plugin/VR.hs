module Plugin.VR where

import           Plugin.Imports
import           Plugin.SimulaController


import           Foreign ( deRefStablePtr
                         , castPtrToStablePtr
                         )

import           Data.Foldable (fold)

import qualified Godot.Gdnative.Internal.Api as Api
import qualified Godot.Methods as G


data VRInitResult
  = InitVRSuccess VRBackend
  | InitVRFailed

data VRBackend
  = OpenHMD OpenHMDConfig
  | OpenVR
 deriving Eq

instance Show VRBackend where
  show OpenVR      = "OpenVR"
  show (OpenHMD _) = "OpenHMD"

newtype OpenHMDConfig = OpenHMDConfig GodotObject
 deriving Eq


openHMDConfig :: IO OpenHMDConfig
openHMDConfig = do
  "res://addons/godot-openhmd/OpenHMDConfig.gdns"
    & unsafeNewNS id "Object" [] -- get configuration object
    <&> OpenHMDConfig


initVR :: VRBackend -> GodotNode -> IO VRInitResult
initVR vri node =
  findInterface >>= G.initialize >>= \case
    True  -> initSuccess
    False -> initFailed
 where
  vriStr = pack $ show vri

  initSuccess :: IO VRInitResult
  initSuccess = do
    G.get_viewport node >>= (`G.set_use_arvr` True)
    godotPrint $ fold ["Initialized ", vriStr]
    return $ InitVRSuccess vri

  initFailed :: IO VRInitResult
  initFailed = do
    godotPrint $ fold ["Failed to initialize ", vriStr, " interface."]
    return InitVRFailed

  findInterface :: IO GodotARVRInterface
  findInterface = do
    arvrServer <- getSingleton GodotARVRServer "ARVRServer"
    G.find_interface arvrServer =<< toLowLevel vriStr


addVRNodes :: GodotNode -> VRBackend -> IO ()
addVRNodes self vri = do
  p <- unsafeInstance GodotARVROrigin "ARVROrigin"
  G.add_child self (asObj p) True

  hmd <- unsafeInstance GodotARVRCamera "ARVRCamera"
  G.add_child self (asObj hmd) True

  when (vri == OpenVR) $ do
    newController p "LeftController"
      >>= (`G.set_controller_id` 1)
    newController p "RightController"
      >>= (`G.set_controller_id` 2)

 where
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

    G.connect ct btnPressed (asObj self) btnSignal argsPressed 0
    G.connect ct btnReleased (safeCast self) btnSignal argsReleased 0

    return ()
