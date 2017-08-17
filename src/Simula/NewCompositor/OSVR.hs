module Simula.NewCompositor.OSVR where

import Control.Lens
import Simula.OSVR

data SimulaOSVRClient = SimulaOSVRClient {
  _simulaOsvrContext :: OSVR_ClientContext,
  _simulaOsvrDisplay :: Maybe OSVR_DisplayConfig
  } deriving Eq

makeLenses ''SimulaOSVRClient

initSimulaOSVRClient :: IO SimulaOSVRClient
initSimulaOSVRClient = do
  ctx <- osvrClientInit "simula.compositor" 0
  return $ SimulaOSVRClient ctx Nothing

newSimulaOSVRClient :: Maybe SimulaOSVRClient -> IO SimulaOSVRClient
newSimulaOSVRClient Nothing = do
  client <- initSimulaOSVRClient
  newSimulaOSVRClient $ Just client

newSimulaOSVRClient (Just client) = do
  let ctx = view simulaOsvrContext client
  (status, display) <- osvrClientGetDisplay ctx
  case status of
    ReturnSuccess -> do
        checkDisplayUntilSuccess ctx display
        return $ SimulaOSVRClient ctx (Just display)
    _ -> ioError $ userError "Could not initialize the display for OSVR"

  where
    checkDisplayUntilSuccess ctx display = do
      ret <- osvrClientCheckDisplayStartup display
      case ret of
        ReturnSuccess -> return ()
        _ -> osvrClientUpdate ctx >> checkDisplayUntilSuccess ctx display
