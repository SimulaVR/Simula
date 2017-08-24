module Simula.Compositor.OSVR where

import Data.Text as T
import Data.Text (Text)

import Control.Lens
import Control.Concurrent.MVar

import Foreign.Ptr
import Foreign.StablePtr

import Simula.OSVR

data SimulaOSVRClient = SimulaOSVRClient {
  _simulaOsvrContext :: OSVR_ClientContext,
  _simulaOsvrDisplay :: Maybe OSVR_DisplayConfig
  } deriving Eq

-- TODO: plug into viewport and surfaces
data Tracker = Tracker {
  _position :: MVar OSVR_Vec3,
  _headOrientation :: MVar OSVR_Quaternion
  -- renderer :: _
  -- model :: _
  -- texture :: _
  -- etc...
  } deriving Eq

makeLenses ''Tracker
makeLenses ''SimulaOSVRClient

initSimulaOSVRClient :: IO SimulaOSVRClient
initSimulaOSVRClient = do
  ctx <- osvrClientInit "simula.compositor" 0
  return $ SimulaOSVRClient ctx Nothing

waitForOsvrDisplay :: Maybe SimulaOSVRClient -> IO SimulaOSVRClient
waitForOsvrDisplay Nothing = do
  client <- initSimulaOSVRClient
  waitForOsvrDisplay $ Just client

waitForOsvrDisplay (Just client) = do
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

interfaceQuery :: SimulaOSVRClient -> Text -> IO (Either OSVR_ReturnCode (Ptr OSVR_ClientInterface))
interfaceQuery ctx@(SimulaOSVRClient osvrCtx _) path = do
    let p = nullPtr
    v <- osvrClientGetInterface osvrCtx (T.unpack path) p
    case v of
      ReturnSuccess -> return $ Right p
      _ -> return $ Left v

setupHeadTracking :: SimulaOSVRClient -> IO ()
setupHeadTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx $ T.pack "/me/head"
    case q of
      Left rc -> ioError $ userError "Could not get interface for head tracking"
      Right p -> do
        callBackStbl <- newStablePtr headTrackingCallback
        registerPoseCallback ctx p callBackStbl

setupLeftHandTracking :: SimulaOSVRClient -> IO ()
setupLeftHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx $ T.pack "/me/hand/left"
    case q of
      Left rc -> ioError $ userError "Could not get interface for left hand"
      Right p -> do
        callBackStbl <- newStablePtr leftHandTrackingCallback
        registerPoseCallback ctx p callBackStbl


setupRightHandTracking :: SimulaOSVRClient -> IO ()
setupRightHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx $ T.pack "/me/hand/right"
    case q of
      Left rc -> ioError $ userError "Could not get interface for right hand"
      Right p -> do
        callBackStbl <- newStablePtr rightHandTrackingCallback
        registerPoseCallback ctx p callBackStbl

registerPoseCallback :: SimulaOSVRClient -> Ptr OSVR_ClientInterface -> StablePtr a -> IO ()
registerPoseCallback ctx@(SimulaOSVRClient osvrCtx _) p callback = do
    deRefP <- deRefStablePtr $ castPtrToStablePtr (castPtr p)
    -- last argument is void *userdata
    osvrRegisterPoseCallback deRefP (castPtrToFunPtr $ castStablePtrToPtr callback) nullPtr
    ret <- osvrClientUpdate osvrCtx
    case ret of
        ReturnSuccess -> putStrLn "Registered tracking callback"
        _             -> putStrLn "Could not register tracking callback"


-- For Pose callbacks the types are:
-- void *userdata
-- const OSVR_TimeValue *timestamp
-- const OSVR_PoseReport *report
--
-- For the HTC Vive and many other setups, Poses are available for a head,
-- a left hand, and a right hand. Each of these can use the
-- osvrRegisterPoseCallback function to react to 
headTrackingCallback :: Ptr a -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
headTrackingCallback iface stamp report = undefined

leftHandTrackingCallback :: Ptr a -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
leftHandTrackingCallback iface stamp report = undefined

rightHandTrackingCallback :: Ptr a -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
rightHandTrackingCallback  iface stamp report = undefined

