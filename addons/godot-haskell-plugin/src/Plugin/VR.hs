{-# LANGUAGE BangPatterns #-}
module Plugin.VR
  ( VRInitResult(..)
  , openHMD
  , openVR
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
  | OpenVR
 deriving Eq

instance Show VRBackend where
  show (OpenHMD _) = "OpenHMD"
  show OpenVR      = "OpenVR"

newtype OpenHMDConfig = OpenHMDConfig GodotObject
 deriving Eq


-- | Initialize and get the OpenHMD configuration object
openHMDConfig :: IO OpenHMDConfig
openHMDConfig = do
  "res://addons/godot-openhmd/OpenHMDConfig.gdns"
    & newNS' []
    <&> OpenHMDConfig


-- | Get the OpenHMD ARVRInterface
openHMD :: IO GodotARVRInterface
openHMD = do
  !cfg <- openHMDConfig
  findInterface $ OpenHMD cfg

-- | Get the OpenVR ARVRInterface
openVR :: IO GodotARVRInterface
openVR = findInterface OpenVR


-- | Initialize the ARVRInterface and return the success/failure
initVR :: GodotNode -> GodotARVRInterface -> IO VRInitResult
initVR node vri =
  G.initialize vri >>= \case
    True  -> initSuccess
    False -> initFailed
 where
  initSuccess :: IO VRInitResult
  initSuccess = do
    getSingleton Godot_OS "OS" >>= (`G.set_use_vsync` False) -- Vsync must be disabled or we're limited to 60fps
    getSingleton Godot_Engine "Engine" >>= (`G.set_target_fps` 90) -- Boost FPS to 90 without vsync

    InitVRSuccess <$ godotPrint "Initialized VR interface."

  initFailed :: IO VRInitResult
  initFailed = do
    InitVRFailed <$ godotPrint "Failed to initialize VR interface."


-- | Find the interface for the given backend
findInterface :: VRBackend -> IO GodotARVRInterface
findInterface vri = do
  vriStr <- toLowLevel $ pack $ show vri
  getSingleton GodotARVRServer "ARVRServer"
    >>= (`G.find_interface` vriStr)
