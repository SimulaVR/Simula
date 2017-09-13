module Simula.Compositor.OSVR where

import Data.Text as T
import Data.Text (Text)

import Control.Lens
import Linear

import Control.Concurrent.MVar

import Foreign
import Foreign.C

import Simula.OSVR
import Simula.Compositor.Utils

data SimulaOSVRClient = SimulaOSVRClient {
  _simulaOsvrContext :: OSVR_ClientContext,
  _simulaOsvrDisplay :: Maybe OSVR_DisplayConfig
  } deriving Eq

-- TODO: plug into viewport and surfaces
data PoseTracker = PoseTracker {
  _interfacePath :: Text,
  _sensorNumber :: Int,
  _position :: MVar (V3 Double),
  _orientation :: MVar (V4 Double)
  -- renderer :: _
  -- model :: _
  -- texture :: _
  -- etc...
  } deriving Eq

mkDefaultPoseTracker :: Int -> Text -> IO PoseTracker
mkDefaultPoseTracker s path =
    PoseTracker <$> return path
                <*> return s
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

osvrGetPose :: OSVR_ClientContext -> Text -> IO PoseTracker
osvrGetPose ctx path = do
  q <- interfaceQuery (SimulaOSVRClient ctx Nothing) path
  case q of
    Left rc -> ioError $ userError "Could not get interface for head pose"
    Right p -> do
      iface <- peek p
      (OSVR_TimeValue tv) <- mkTimeValue
      p@(OSVR_PoseReport ps) <- mkPose
      r <- osvrGetPoseState iface tv ps
      case toEnum r of
            ReturnSuccess -> do
              prP <- new ps
              s <- getSensorFromReport p
              pt <- mkDefaultPoseTracker s path
              pos <- getTranslationFromReport p >>= mkV3fromVec3
              writeMVar (pt ^. position) pos
              rot <- getRotationFromReport p >>= mkV4fromQuat
              writeMVar (pt ^. orientation) rot
              free tv >> free ps >> return pt
            _ -> do
              free tv >> free ps
              ioError $ userError ("Could not fetch pose from " ++ pathStr)
  where
    pathStr = T.unpack path

osvrGetHeadPose :: OSVR_ClientContext -> IO PoseTracker
osvrGetHeadPose ctx = osvrGetPose ctx "/me/head"

osvrGetLeftHandPose :: OSVR_ClientContext -> IO PoseTracker
osvrGetLeftHandPose ctx = osvrGetPose ctx "/me/hands/left"

osvrGetRightHandPose :: OSVR_ClientContext -> IO PoseTracker
osvrGetRightHandPose ctx = osvrGetPose ctx "/me/hands/right"

setupHeadTracking :: SimulaOSVRClient -> IO ()
setupHeadTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for head tracking"
      Right p -> do
        userdata <- mkDefaultPoseTracker (0-1) path >>= newStablePtr
        registerPoseCallback ctx p poseTrackingCallback userdata
  where
    path = "/me/head"

setupLeftHandTracking :: SimulaOSVRClient -> IO ()
setupLeftHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for left hand"
      Right p -> do
        userdata <- mkDefaultPoseTracker (0-1) path >>= newStablePtr
        registerPoseCallback ctx p poseTrackingCallback userdata
  where
    path = "/me/hands/left"

setupRightHandTracking :: SimulaOSVRClient -> IO ()
setupRightHandTracking ctx@(SimulaOSVRClient osvrCtx _) = do
    q <- interfaceQuery ctx path
    case q of
      Left rc -> ioError $ userError "Could not get interface for right hand"
      Right p -> do
        userdata <- mkDefaultPoseTracker (0-1) path >>= newStablePtr
        registerPoseCallback ctx p poseTrackingCallback userdata
  where
    path = "/me/hands/right"

type PoseCallback = Ptr PoseTracker -> OSVR_TimeValue -> OSVR_PoseReport -> IO ()
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
poseTrackingCallback :: Ptr PoseTracker -> OSVR_TimeValue -> OSVR_PoseReport -> IO ()
poseTrackingCallback userdata stamp report = do
    (sec, usec) <- timeValuePair stamp
    sensor <- getSensorFromReport report
    p <- getTranslationFromReport report
    r <- getRotationFromReport report

    -- TODO: stuff with the data, maybe even the userdata?
    posRep <- positionReport p
    rotRep <- rotationReport r
    putStrLn $ "[DEBUG] sensor id " ++ show sensor
            ++ " reported at " ++ show sec ++ ":" ++ show usec
            ++ " with position " ++ posRep
            ++ " and rotation " ++ rotRep

positionReport :: OSVRVec3 -> IO String
positionReport (OSVRVec3 p) = do
    x <- with p vec3GetX
    y <- with p vec3GetY
    z <- with p vec3GetZ
    return $ ">" ++ show x ++ " ^" ++ show y ++ " ·" ++ show z

rotationReport :: OSVRQuaternion -> IO String
rotationReport (OSVRQuaternion r) = do
    w <- with r quatGetW
    x <- with r quatGetX
    y <- with r quatGetY
    z <- with r quatGetZ
    return $ "w" ++ show w ++ ">" ++ show x ++ " ^" ++ show y ++ " ·" ++ show z

timeValuePair :: OSVR_TimeValue -> IO (Int, Int)
timeValuePair stamp = do
    sec <- getSeconds stamp
    usec <- getMicroseconds stamp
    return (sec, usec)
