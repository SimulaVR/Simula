module Simula.Compositor.OSVR where

import Data.Text as T
import Data.Text (Text)

import Control.Lens
import Linear

import Control.Concurrent.MVar

import Foreign

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
    p <- mallocForeignPtr
    withForeignPtr p $
      \rawPtr -> do
        v <- osvrClientGetInterface osvrCtx (T.unpack path) rawPtr
        case v of
          ReturnSuccess -> return $ Right rawPtr
          _ -> return $ Left v

setupHeadTracking :: SimulaOSVRClient -> IO ()
setupHeadTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for head tracking"
      Right p -> do
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p headTrackingCallback userdata
  where
    path = "/me/head"

setupLeftHandTracking :: SimulaOSVRClient -> IO ()
setupLeftHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for left hand"
      Right p -> do
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p leftHandTrackingCallback userdata
  where
    path = "/me/hands/left"

setupRightHandTracking :: SimulaOSVRClient -> IO ()
setupRightHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for right hand"
      Right p -> do
        userdata <- mkDefaultPoseTracker path >>= newStablePtr
        registerPoseCallback ctx p rightHandTrackingCallback userdata
  where
    path = "/me/hands/right"

type PoseCallback = Ptr PoseTracker -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
foreign import ccall "wrapper"
  mkPoseCallback :: PoseCallback -> IO (FunPtr PoseCallback)

registerPoseCallback :: SimulaOSVRClient
                     -> Ptr OSVR_ClientInterface
                     -> PoseCallback
                     -> StablePtr PoseTracker
                     -> IO ()
registerPoseCallback ctx@(SimulaOSVRClient osvrCtx _) p callback userdata = do
    u <- deRefStablePtr userdata
    client <- peekElemOff p 0
    cf <- mkPoseCallback callback
    osvrRegisterPoseCallback client (castFunPtr cf) (castStablePtrToPtr userdata)

    ret <- osvrClientUpdate osvrCtx
    case ret of
        ReturnSuccess -> putStrLn $ "Registered tracking callback: " ++ (T.unpack $ _interfacePath u)
        _             -> putStrLn $ "Could not register tracking callback" ++ (T.unpack $ _interfacePath u)

-- For Pose callbacks the types are:
-- void *userdata
-- const OSVR_TimeValue *timestamp
-- const OSVR_PoseReport *report
--
-- For the HTC Vive and many other setups, Poses are available for a head,
-- a left hand, and a right hand. Each of these can use the
-- osvrRegisterPoseCallback function to react to 
headTrackingCallback :: Ptr PoseTracker -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
headTrackingCallback userdata stamp report = do
    (sec, usec) <- timeValuePair stamp
    putStrLn $ "[DEBUG] left hand reported at " ++ show sec ++ ":" ++ show usec
--            ++ " with position " ++ positionReport
 --           ++ " with orientation " ++ orientationReport

leftHandTrackingCallback :: Ptr PoseTracker -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
leftHandTrackingCallback userdata stamp report = do
    (sec, usec) <- timeValuePair stamp
    putStrLn $ "[DEBUG] left hand reported at " ++ show sec ++ ":" ++ show usec
--            ++ " with position " ++ positionReport
 --           ++ " with orientation " ++ orientationReport


rightHandTrackingCallback :: Ptr PoseTracker -> Ptr OSVR_TimeValue -> Ptr OSVR_Pose3 -> IO ()
rightHandTrackingCallback  userdata stamp report = do
    (sec, usec) <- timeValuePair stamp
    putStrLn $ "[DEBUG] left hand reported at " ++ show sec ++ ":" ++ show usec
--            ++ " with position " ++ positionReport
 --           ++ " with orientation " ++ orientationReport

timeValuePair :: Ptr OSVR_TimeValue -> IO (OSVR_TimeValue_Seconds, OSVR_TimeValue_Microseconds)
timeValuePair stamp = do
    t <- peekElemOff stamp 0
    sec <- getSeconds t
    usec <- getMicroseconds t
    return (sec, usec)
