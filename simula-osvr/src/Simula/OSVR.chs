{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simula.OSVR where

import Control.Monad
import Data.Proxy
import Data.Word
import System.Posix.DynamicLinker
import Foreign
import Foreign.C
import Linear

#include <osvr/ClientKit/ClientKitC.h>
#include <osvr/ClientKit/DisplayC.h>
  
{#pointer OSVR_ClientContext newtype#}
{#pointer OSVR_DisplayConfig newtype#}
deriving instance Storable OSVR_DisplayConfig
{#pointer *OSVR_Pose3 newtype#}

{#enum define OSVR_ReturnCode {OSVR_RETURN_SUCCESS as ReturnSuccess, OSVR_RETURN_FAILURE as ReturnFailure} deriving (Eq)#}


type OSVR_ChannelCount = {#type OSVR_ChannelCount#}
{#typedef OSVR_ChannelCount OSVR_ChannelCount#}

type OSVR_DisplayInputCount = {#type OSVR_DisplayInputCount#}
{#typedef OSVR_DisplayInputCount OSVR_DisplayInputCount#}

type OSVR_ViewerCount = {#type OSVR_ViewerCount#}
{#typedef OSVR_ViewerCount OSVR_ViewerCount#}

type OSVR_DisplayDimension = {#type OSVR_DisplayDimension#}
{#typedef OSVR_DisplayDimension OSVR_DisplayDimension#}

type OSVR_EyeCount = {#type OSVR_EyeCount#}
{#typedef OSVR_EyeCount OSVR_EyeCount#}

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
                              , alloca- `OSVR_ViewerCount' peek*} -> `OSVR_ReturnCode'#}

{#fun osvrClientGetViewerPose {`OSVR_DisplayConfig'
                              , `OSVR_ViewerCount'
                              , `OSVR_Pose3'} -> `OSVR_ReturnCode'#}

{#fun osvrClientGetNumEyesForViewer {`OSVR_DisplayConfig'
                                    , `OSVR_ViewerCount'
                                    , alloca- `OSVR_EyeCount' peek*} -> `OSVR_ReturnCode'#}

{#fun osvrClientGetViewerEyePose {`OSVR_DisplayConfig'
                                 , `OSVR_ViewerCount'
                                 , `OSVR_EyeCount'
                                 , `OSVR_Pose3'} -> `OSVR_ReturnCode'#}

{#enum OSVR_MatrixOrderingFlags {underscoreToCase}  #}

{#fun osvrClientGetViewerEyeViewMatrixd {`OSVR_DisplayConfig'
                                        , `OSVR_ViewerCount'
                                        , `OSVR_EyeCount'
                                        , `OSVR_MatrixOrderingFlags'
                                        , id `Ptr CDouble' } -> `OSVR_ReturnCode'#}

osvrClientGetViewerEyeViewMatrixd' :: OSVR_DisplayConfig
                                   -> OSVR_ViewerCount
                                   -> OSVR_EyeCount
                                   -> IO (M44 Double)
osvrClientGetViewerEyeViewMatrixd' disp viewer eye = alloca $ \matPtr -> do
  osvrClientGetViewerEyeViewMatrixd disp viewer eye OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr

{#fun osvrClientGetNumSurfacesForViewerEye {`OSVR_DisplayConfig'
                                           , `OSVR_ViewerCount'
                                           , `OSVR_EyeCount'
                                           , alloca- `OSVR_SurfaceCount' peek*} -> `OSVR_ReturnCode'#}
                                           
{#fun osvrClientGetRelativeViewportForViewerEyeSurface {`OSVR_DisplayConfig'
                                                       , `OSVR_ViewerCount'
                                                       , `OSVR_EyeCount'
                                                       , `OSVR_SurfaceCount'
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*
                                                       , alloca- `CInt' peek*} -> `OSVR_ReturnCode'#}
{#fun osvrClientGetViewerEyeSurfaceProjectionMatrixd {`OSVR_DisplayConfig'
                                                     , `OSVR_ViewerCount'
                                                     , `OSVR_EyeCount'
                                                     , `OSVR_SurfaceCount'
                                                     , `Double'
                                                     , `Double'
                                                     , `OSVR_MatrixOrderingFlags'
                                                     , id `Ptr CDouble' } -> `OSVR_ReturnCode'#}

osvrClientGetViewerEyeSurfaceProjectionMatrixd' :: OSVR_DisplayConfig
                                                -> OSVR_ViewerCount
                                                -> OSVR_EyeCount
                                                -> OSVR_SurfaceCount
                                                -> Double
                                                -> Double
                                                -> IO (M44 Double)
osvrClientGetViewerEyeSurfaceProjectionMatrixd' disp viewer eye surf near far = alloca $ \matPtr -> do
  osvrClientGetViewerEyeSurfaceProjectionMatrixd disp viewer eye surf near far OsvrMatrixRowmajor (castPtr matPtr)
  peek matPtr
