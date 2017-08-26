{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Simula.OSVR where

import GHC.Generics (Generic)
import System.IO
import Control.Monad
import Data.Proxy
import Data.Word
import System.Posix.DynamicLinker
import Foreign
import Foreign.C
import Linear

#include <osvr/ClientKit/ClientKitC.h>
#include <osvr/ClientKit/DisplayC.h>
#include <osvr/ClientKit/InterfaceC.h>
#include <osvr/Util/TimeValueC.h>
#include <osvr/Util/Vec3C.h>
#include <osvr/Util/QuaternionC.h>
#include <osvr/Util/Pose3C.h>
#include <osvr/Util/ClientReportTypesC.h>

-- #include <osvr/RenderKit/RenderManagerC.h>
-- #include <osvr/RenderKit/RenderManagerOpenGLC.h>

{#pointer OSVR_ClientContext newtype#}
deriving instance Eq OSVR_ClientContext

{#pointer OSVR_DisplayConfig newtype#}
deriving instance Eq OSVR_DisplayConfig
deriving instance Storable OSVR_DisplayConfig

{#pointer OSVR_ClientInterface newtype #}
deriving instance Eq OSVR_ClientInterface
deriving instance Storable OSVR_ClientInterface

{#enum define OSVR_ReturnCode {OSVR_RETURN_SUCCESS as ReturnSuccess, OSVR_RETURN_FAILURE as ReturnFailure} deriving (Eq)#}

{#pointer *OSVR_TimeValue newtype#}
deriving instance Eq OSVR_TimeValue

getSeconds :: OSVR_TimeValue -> IO Int
getSeconds tv = do
   cseconds <- {#get OSVR_TimeValue.seconds #} tv
   return $ fromIntegral cseconds

getMicroseconds :: OSVR_TimeValue -> IO Int
getMicroseconds tv = do
    cmicro <- {#get OSVR_TimeValue.microseconds #} tv
    return $ fromIntegral cmicro

data PoseState = PoseState { _translation :: V3 Double
                           , _rotation :: V4 Double
                           } deriving (Show, Generic, Eq)
instance Storable PoseState where
    sizeOf _ = {#sizeof OSVR_Pose3 #}
    alignment _ = {#alignof OSVR_Pose3 #}
    peekElemOff pr idx = fixIO $ \result ->
        peek (pr `plusPtr` (idx * sizeOf result))
    pokeElemOff pr idx x = poke (pr `plusPtr` (idx * sizeOf x)) x

{#pointer *OSVR_PoseState as OSVR_PoseState -> PoseState #}
{#pointer *OSVR_Pose3 -> PoseState #}

data PoseReport = PoseReport { _sensorid :: Int
                             , _pose :: PoseState
                             } deriving (Show, Generic, Eq)
instance Storable PoseReport where
    sizeOf _ = {#sizeof OSVR_PoseReport #}
    alignment _ = {#alignof OSVR_PoseReport #}
    peekElemOff pr idx = fixIO $ \result ->
        peek (pr `plusPtr` (idx * sizeOf result))
    pokeElemOff pr idx x = poke (pr `plusPtr` (idx * sizeOf x)) x


{#pointer *OSVR_PoseReport as OSVR_PoseReport -> PoseReport #}

data PoseVec3 = PoseVec3 { _vData :: V3 Double }
    deriving (Eq, Show)
{#pointer *OSVR_Vec3 as OSVR_Vec3 -> PoseVec3 #}

data PoseQuat = PoseQuat { _qData :: V4 Double }
{#pointer *OSVR_Quaternion as OSVR_Quaternion -> PoseQuat #}

getSensorFromReport :: OSVR_PoseReport -> IO Int
getSensorFromReport r = do
    s <- {#get struct OSVR_PoseReport -> sensor #} r
    return $ fromIntegral s

getPoseFromReport :: OSVR_PoseReport -> IO PoseState
getPoseFromReport pr = do
    p <- {#get struct OSVR_PoseReport -> pose #} pr
    v <- peek (castPtr p)
    return v

getPosistionFromPose :: PoseState -> V3 Double
getPosistionFromPose (PoseState t _) = t

getOrientFromPose :: PoseState -> V4 Double
getOrientFromPose (PoseState _ r) = r

type OSVR_ChannelCount = {#type OSVR_ChannelCount#}
{#typedef OSVR_ChannelCount OSVR_ChannelCount#}

type OSVR_DisplayInputCount = {#type OSVR_DisplayInputCount#}
{#typedef OSVR_DisplayInputCount OSVR_DisplayInputCount#}

type OSVR_DisplayDimension = {#type OSVR_DisplayDimension#}
{#typedef OSVR_DisplayDimension OSVR_DisplayDimension#}

type OSVR_SurfaceCount = {#type OSVR_SurfaceCount#}
{#typedef OSVR_SurfaceCount OSVR_SurfaceCount#}

{#fun osvrClientInit {`String',`Word32'} -> `OSVR_ClientContext'#}
{#fun osvrClientUpdate {`OSVR_ClientContext'} -> `OSVR_ReturnCode'#}
{#fun osvrClientCheckStatus {`OSVR_ClientContext'} -> `OSVR_ReturnCode'#}
{#fun osvrClientShutdown {`OSVR_ClientContext'} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetDisplay {`OSVR_ClientContext', alloca- `OSVR_DisplayConfig' peek*} -> `OSVR_ReturnCode'#}
{#fun osvrClientFreeDisplay {`OSVR_DisplayConfig'} -> `OSVR_ReturnCode'#}
{#fun osvrClientCheckDisplayStartup {`OSVR_DisplayConfig'} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetNumDisplayInputs {`OSVR_DisplayConfig'
                                    , alloca- `OSVR_DisplayInputCount' peek*} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetDisplayDimensions {`OSVR_DisplayConfig'
                                     , `OSVR_DisplayInputCount'
                                     , alloca- `OSVR_DisplayDimension' peek*
                                     , alloca- `OSVR_DisplayDimension' peek*} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetNumViewers {`OSVR_DisplayConfig'
                              , alloca- `CUInt' peek*}  -- Viewers/as Count
                                  -> `OSVR_ReturnCode'#}

{#fun osvrClientGetViewerPose {`OSVR_DisplayConfig'
                              , `CInt' -- Viewers/as ID
                              , `OSVR_Pose3'} -> `OSVR_ReturnCode'#}

{#fun osvrClientGetNumEyesForViewer {`OSVR_DisplayConfig'
                                    , `CUInt'                 -- Viewers/as ID
                                    , alloca- `CUChar' peek*} -- EyeCount/as Count
                                        -> `OSVR_ReturnCode'#}

{#fun osvrClientGetViewerEyePose {`OSVR_DisplayConfig'
                                 , `CUInt' -- Viewers/as ID
                                 , `CUInt' -- EyeCount/as ID
                                 , `OSVR_Pose3'} -> `OSVR_ReturnCode'#}

{#fun osvrClientGetInterface { `OSVR_ClientContext'
                             , `String' -- interface path
                             , id `Ptr OSVR_ClientInterface' } -> `OSVR_ReturnCode' #}

{#fun osvrRegisterPoseCallback { `OSVR_ClientInterface'
                               , id `FunPtr (Ptr () -> OSVR_TimeValue -> OSVR_PoseReport -> IO ())'
                               , id `Ptr ()' } -> `()' #}

{#enum OSVR_MatrixOrderingFlags {underscoreToCase} deriving (Show, Eq)#}

{#fun osvrClientGetViewerEyeViewMatrixd {`OSVR_DisplayConfig'
                                        , `CUInt' -- Viewers/as ID
                                        , `CUInt' -- EyeCount/as ID
                                        , `OSVR_MatrixOrderingFlags'
                                        , id `Ptr CDouble' } -> `OSVR_ReturnCode'#}

osvrClientGetViewerEyeViewMatrixd' :: OSVR_DisplayConfig
                                   -> CUInt  -- Viewers/as ID
                                   -> CUInt -- EyeCount/as ID
                                   -> IO (M44 Double)
osvrClientGetViewerEyeViewMatrixd' disp viewer eye = alloca $ \matPtr -> do
  osvrClientGetViewerEyeViewMatrixd disp viewer eye OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr

{#fun osvrClientGetViewerEyeViewMatrixf {`OSVR_DisplayConfig'
                                        , `CUInt' -- Viewers/as ID
                                        , `Word8' -- EyeCount/as ID
                                        , `OSVR_MatrixOrderingFlags'
                                        , id `Ptr CFloat' } -> `OSVR_ReturnCode'#}


osvrClientGetViewerEyeViewMatrixf' :: OSVR_DisplayConfig
                                   -> CUInt -- Viewers/as ID
                                   -> Word8 -- EyeCount/as ID
                                   -> IO (M44 Float)
osvrClientGetViewerEyeViewMatrixf' disp viewer eye = alloca $ \matPtr -> do
  osvrClientGetViewerEyeViewMatrixf disp viewer eye OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr

{#fun osvrClientGetNumSurfacesForViewerEye {`OSVR_DisplayConfig'
                                           , `CUInt'  -- Viewers/as ID
                                           , `CUInt' -- EyeCount/as ID
                                           , alloca- `OSVR_SurfaceCount' peek*} -> `OSVR_ReturnCode'#}
                                           
{#fun osvrClientGetRelativeViewportForViewerEyeSurface {`OSVR_DisplayConfig'
                                                       , `CUInt'  -- Viewers/as ID
                                                       , `CUInt' -- EyeCount/as ID
                                                       , `OSVR_SurfaceCount'
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetViewerEyeSurfaceProjectionMatrixd {`OSVR_DisplayConfig'
                                                     , `CUInt'  -- Viewers/as ID
                                                     , `CUInt' -- EyeCount/as ID
                                                     , `OSVR_SurfaceCount'
                                                     , `Double'
                                                     , `Double'
                                                     , `OSVR_MatrixOrderingFlags'
                                                     , id `Ptr CDouble' } -> `OSVR_ReturnCode'#}

osvrClientGetViewerEyeSurfaceProjectionMatrixd' :: OSVR_DisplayConfig
                                                -> CUInt -- Viewers/as ID
                                                -> CUInt -- EyeCount/as ID
                                                -> OSVR_SurfaceCount
                                                -> Double
                                                -> Double
                                                -> IO (M44 Double)
osvrClientGetViewerEyeSurfaceProjectionMatrixd' disp viewer eye surf near far = alloca $ \matPtr -> do
  osvrClientGetViewerEyeSurfaceProjectionMatrixd disp viewer eye surf near far OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr

{#fun osvrClientGetViewerEyeSurfaceProjectionMatrixf {`OSVR_DisplayConfig'
                                                     , `CUInt' -- Viewers/as ID
                                                     , `CUInt' -- EyeCount/as ID
                                                     , `OSVR_SurfaceCount'
                                                     , `Float'
                                                     , `Float'
                                                     , `OSVR_MatrixOrderingFlags'
                                                     , id `Ptr CFloat' } -> `OSVR_ReturnCode'#}

osvrClientGetViewerEyeSurfaceProjectionMatrixf' :: OSVR_DisplayConfig
                                                -> CUInt -- Viewers/as ID
                                                -> CUInt -- EyeCount/as ID
                                                -> OSVR_SurfaceCount
                                                -> Float
                                                -> Float
                                                -> IO (M44 Float)
osvrClientGetViewerEyeSurfaceProjectionMatrixf' disp viewer eye surf near far = alloca $ \matPtr -> do
  osvrClientGetViewerEyeSurfaceProjectionMatrixf disp viewer eye surf near far OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr


{-

{#pointer *OSVR_OpenResultsOpenGL newtype#}
{#pointer OSVR_RenderManager newtype#}
deriving instance Storable OSVR_RenderManager
{#pointer OSVR_RenderManagerOpenGL newtype#}
deriving instance Storable OSVR_RenderManagerOpenGL

{#fun osvrCreateRenderManagerOpenGL {`OSVR_ClientContext'
                                    , `String'
                                    , id `Ptr ()'
                                    , alloca- `OSVR_RenderManager' peek*
                                    , alloca- `OSVR_RenderManagerOpenGL' peek* } -> `OSVR_ReturnCode'#}

{#fun osvrRenderManagerOpenDisplayOpenGL {`OSVR_RenderManagerOpenGL', `OSVR_OpenResultsOpenGL'} -> `OSVR_ReturnCode' #}

osvrRenderManagerOpenDisplayOpenGL' :: OSVR_RenderManagerOpenGL -> IO (OSVR_ReturnCode, OSVR_OpenResultsOpenGL)
osvrRenderManagerOpenDisplayOpenGL' rmgl = do
  res <- OSVR_OpenResultsOpenGL <$> mallocBytes {#sizeof OSVR_OpenResultsOpenGL#}
  ret <- osvrRenderManagerOpenDisplayOpenGL rmgl res
  return (ret, res)
-}

-- Callbacks
