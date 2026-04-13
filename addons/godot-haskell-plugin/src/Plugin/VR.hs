{-# LANGUAGE BangPatterns #-}
module Plugin.VR
  ( VRInitResult(..)
  , openHMD
  , openXR
  , initVR
  ) where

import           Plugin.Imports
import           Plugin.Types

import qualified Godot.Methods as G


data VRInitResult
  = InitVRSuccess
  | InitVRFailed

data VRBackend
  = OpenHMD OpenHMDConfig
  | OpenXR
 deriving Eq

instance Show VRBackend where
  show (OpenHMD _) = "OpenHMD"
  show OpenXR      = "OpenXR"

newtype OpenHMDConfig = OpenHMDConfig GodotObject
 deriving Eq


-- | Initialize and get the OpenHMD configuration object
openHMDConfig :: IO OpenHMDConfig
openHMDConfig = do
  debugPutStrLn "Plugin.VR.openHMDConfig"
  "res://addons/godot-openhmd/OpenHMDConfig.gdns"
    & newNS' []
    <&> OpenHMDConfig


-- | Get the OpenHMD ARVRInterface
openHMD :: IO GodotARVRInterface
openHMD = do
  debugPutStrLn "Plugin.VR.openHMD"
  !cfg <- openHMDConfig
  findInterface $ OpenHMD cfg

-- | Get the OpenXR ARVRInterface
openXR :: IO GodotARVRInterface
openXR = do
  debugPutStrLn "Plugin.VR.openXR"
  findInterface OpenXR

-- | Initialize the ARVRInterface and return the success/failure
initVR :: GodotNode -> GodotARVRInterface -> IO VRInitResult
initVR node vri = do
  debugPutStrLn "Plugin.VR.initVR"
  case validateObject vri of
    Nothing -> initFailed
    Just _ -> G.initialize vri >>= \case
      True  -> initSuccess
      False -> initFailed
 where
  initSuccess :: IO VRInitResult
  initSuccess = do
    debugPutStrLn "Plugin.VR.initSuccess"
    getSingleton Godot_OS "OS" >>= (`G.set_use_vsync` False) -- Vsync must be disabled or we're limited to 60fps
    getSingleton Godot_Engine "Engine" >>= (`G.set_target_fps` 0) -- Setting this to constants other than 0 messes with SteamVR Hz settings, and can cause jitters

    InitVRSuccess <$ godotPrint "Initialized VR interface."

  initFailed :: IO VRInitResult
  initFailed = do
    debugPutStrLn "Plugin.VR.initFailed"
    InitVRFailed <$ godotPrint "Failed to initialize VR interface."

-- | Find the interface for the given backend
findInterface :: VRBackend -> IO GodotARVRInterface
findInterface vri = do
  debugPutStrLn "Plugin.VR.findInterface"
  putStrLn $ "Loading VR backend: " ++ show vri
  vriStr <- toLowLevel $ pack $ show vri
  getSingleton GodotARVRServer "ARVRServer"
    >>= (`G.find_interface` vriStr)
