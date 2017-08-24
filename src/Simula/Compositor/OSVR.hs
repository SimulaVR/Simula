module Simula.Compositor.OSVR where

import Data.Text as T
import Data.Text (Text)

import Control.Lens
import Linear

import Control.Concurrent.MVar

import Foreign.Ptr
import Foreign.StablePtr

import Simula.OSVR

data SimulaOSVRClient = SimulaOSVRClient {
  _simulaOsvrContext :: OSVR_ClientContext,
  _simulaOsvrDisplay :: Maybe OSVR_DisplayConfig
  } deriving Eq

-- TODO: plug into viewport and surfaces
data PoseTracker = PoseTracker {
  _interfacePath :: Text,
  _position :: MVar (V3 Double),
  _orientation :: MVar (V4 Double)
  -- renderer :: _
  -- model :: _
  -- texture :: _
  -- etc...
  } deriving Eq

mkDefaultPoseTracker :: Text -> IO PoseTracker
mkDefaultPoseTracker path =
    PoseTracker <$> return path
                <*> newMVar (V3 0.0 0.0 0.0)
                <*> newMVar (V4 0 0.0 0.0 0.0)

makeLenses ''PoseTracker
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
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for head tracking"
      Right p -> do
        callback <- newStablePtr headTrackingCallback
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p callback userdata
  where
    path = "/me/head"

setupLeftHandTracking :: SimulaOSVRClient -> IO ()
setupLeftHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for left hand"
      Right p -> do
        callback <- newStablePtr leftHandTrackingCallback
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p callback userdata
  where
    path = "/me/hand/left"


setupRightHandTracking :: SimulaOSVRClient -> IO ()
setupRightHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for right hand"
      Right p -> do
        callback <- newStablePtr rightHandTrackingCallback
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p callback userdata
  where
    path = "/me/hand/right"

registerPoseCallback :: SimulaOSVRClient -> Ptr OSVR_ClientInterface
                     -> StablePtr a -> StablePtr PoseTracker
                     -> IO ()
registerPoseCallback ctx@(SimulaOSVRClient osvrCtx _) p callback userdata = do
    u <- deRefStablePtr userdata
    client <- deRefStablePtr $ castPtrToStablePtr (castPtr p)

    osvrRegisterPoseCallback client
      (castPtrToFunPtr $ castStablePtrToPtr callback)
      (castStablePtrToPtr userdata)

    ret <- osvrClientUpdate osvrCtx
    case ret of
        ReturnSuccess -> putStrLn $ "Registered tracking callback: " ++ (T.unpack $ _interfacePath u)
        _             -> putStrLn $ "Could not register tracking callback" ++ (T.unpack $ _interfacePath u)
  where

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

