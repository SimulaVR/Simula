module Simula.NewCompositor.OSVR where

import Control.Lens
import Simula.OSVR

data SimulaOSVRClient = SimulaOSVRClient {
  _simulaOsvrContext :: OSVR_ClientContext,
  _simulaOsvrDisplay :: OSVR_DisplayConfig
  } deriving Eq

makeLenses ''SimulaOSVRClient

newSimulaOSVRClient :: IO SimulaOSVRClient
newSimulaOSVRClient = do
  ctx <- osvrClientInit "simula.compositor" 0
  (status, display) <- osvrClientGetDisplay ctx
  case status of
    ReturnSuccess -> do
        checkDisplayUntilSuccess ctx display
        return $ SimulaOSVRClient ctx display
    _ -> ioError $ userError "Could not initialize the display for OSVR"

  where
    checkDisplayUntilSuccess ctx display = do
      ret <- osvrClientCheckDisplayStartup display
      case ret of
        ReturnSuccess -> return ()
        _ -> osvrClientUpdate ctx >> checkDisplayUntilSuccess ctx display
