{-# LANGUAGE DuplicateRecordFields #-}
module Simula.ViveCompositor.ViveCompositor where

import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent.MVar
import Data.Coerce
import Data.Function (on)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Word
import Data.Typeable
import Data.Maybe
import Data.List
import qualified Data.Vector.Storable.Sized as VF
import Data.Void
import Foreign hiding (void)
import Foreign.C
import Foreign.Ptr
import Graphics.Rendering.OpenGL hiding (scale, translate, rotate, lookAt, Rect)
import Graphics.GL
import Linear
import Linear.OpenGL
import System.Clock
import System.Environment
import System.Mem.StableName
import System.Posix.Signals
import Simula.WaylandServer
import Simula.Weston
import Simula.WestonDesktop

import Simula.BaseCompositor.Compositor
import Simula.BaseCompositor.Geometry
import Simula.BaseCompositor.OpenGL
import Simula.BaseCompositor.SceneGraph
import Simula.BaseCompositor.SceneGraph.Wayland
import Simula.BaseCompositor.Wayland.Input
import Simula.BaseCompositor.Wayland.Output
import Simula.BaseCompositor.WindowManager
import Simula.BaseCompositor.Utils
import Simula.BaseCompositor.Types
import Simula.BaseCompositor.Weston hiding (moveCamera)

import Simula.ViveCompositor.SimulaVRModel

import OpenVR
import Graphics.Vulkan


data VulkanInfo  = VulkanInfo {
  _vulkanInstance :: VkInstance,
  _vulkanPhysicalDevice :: VkPhysicalDevice,
  _vulkanDevice :: VkDevice,
  _vulkanQueue :: VkQueue,
  _vulkanQueueFamilyIndex :: Word32,
  _vulkanCommandPool :: VkCommandPool,
  _vulkanCommandBuffer :: VkCommandBuffer
 }

data VulkanImage = VulkanImage {
  _imageImage :: VkImage,
  _imageSemaphore :: VkSemaphore,
  _imageImageMemory :: VkDeviceMemory,
  _imageImageSize :: VkDeviceSize,
  _imageTexture :: TextureObject,
  _imageOpenGLMemoryObject :: GLuint,
  _imageOpenGLSemaphoreObject :: GLuint,
  _imageExtents :: VkExtent3D
}

data ViveCompositor = ViveCompositor {
  _viveCompositorBaseCompositor :: BaseCompositor,
  _viveCompositorVulkanInfo :: VulkanInfo,
  _viveCompositorVulkanImage :: VulkanImage,
  _viveCompositorModels :: MVar (M.Map TrackedDeviceIndex SimulaVRModel),
  _viveCompositorControllers :: MVar (M.Map TrackedDeviceIndex WestonPointer),
  _viveCompositorTargetedWindows :: MVar (M.Map TrackedDeviceIndex ((M44 Float, M44 Float), RaySurfaceIntersection)),
  _viveCompositorIsResizing :: MVar Bool
}

makeLenses ''VulkanInfo
makeLenses ''VulkanImage
makeLenses ''ViveCompositor

newVulkanInfo :: Bool -> IO VulkanInfo
newVulkanInfo verbose = do
  iexts <- ("VK_EXT_debug_report":) <$> ivrCompositorGetVulkanInstanceExtensionsRequired
  iextPtrs <- mapM newCString iexts
  print iexts

  let valLayers = if verbose then
                  [ "VK_LAYER_LUNARG_parameter_validation"
                  , "VK_LAYER_LUNARG_core_validation"
                  , "VK_LAYER_LUNARG_object_tracker"
                  , "VK_LAYER_LUNARG_standard_validation"
                  , "VK_LAYER_GOOGLE_threading"]
                  else []
  valLayerPtrs <- mapM newCString valLayers

  callbackPtr <- createDebugCallbackPtr $ \_ _ _ _ _ _ message _ -> peekCString message >>= print >> return (VkBool32 VK_FALSE)

  
  inst <- createInstance iextPtrs valLayerPtrs callbackPtr
  mapM_ free iextPtrs
  mapM_ free valLayerPtrs

  when verbose $ void $ createDebugReport inst callbackPtr
  
  phys <- findPhysicalDevice inst

  Just idx <- alloca $ \numPtr -> do
    num <- vkGetPhysicalDeviceQueueFamilyProperties phys numPtr nullPtr >> peek numPtr
    props <- allocaArray (fromIntegral num) $ \arrayPtr -> vkGetPhysicalDeviceQueueFamilyProperties phys numPtr arrayPtr >> peekArray (fromIntegral num) arrayPtr
    return $ findIndex (\(VkQueueFamilyProperties flags _ _ _) -> flags .&. VK_QUEUE_GRAPHICS_BIT == VK_QUEUE_GRAPHICS_BIT) props

  let family = fromIntegral idx

  dexts <- ivrCompositorGetVulkanDeviceExtensionsRequired (castPtr phys)
  print dexts
  dextPtrs <- mapM newCString dexts
  dev <- createDevice inst phys dextPtrs family
  mapM_  free dextPtrs
  queue <- createQueue phys dev family
  pool <- createCommandPool dev family
  cmdBuffer <- createCommandBuffer dev pool
  return $ VulkanInfo inst phys dev queue family pool cmdBuffer

  where
    createInstance iexts layers callbackPtr = withCString "vive-compositor" $ \namePtr ->
      with VkApplicationInfo { vkSType = VK_STRUCTURE_TYPE_APPLICATION_INFO
                             , vkPNext = nullPtr
                             , vkPApplicationName = namePtr
                             , vkApplicationVersion = 1
                             , vkPEngineName = namePtr
                             , vkEngineVersion = 0
                             , vkApiVersion = vkMakeVersion 1 0 61
                             } $ \appInfo ->
      withArrayLen iexts $ \extCount extPtr ->
      withArrayLen layers $ \layerCount layerPtr ->
      with VkDebugReportCallbackCreateInfoEXT { vkSType = VkStructureType 1000011000
                                              , vkPNext = nullPtr
                                              , vkFlags = VK_DEBUG_REPORT_ERROR_BIT_EXT .|. VK_DEBUG_REPORT_WARNING_BIT_EXT
                                              , vkPfnCallback = callbackPtr
                                              , vkPUserData = nullPtr
                                              } $ \callbackInfo ->
      with VkInstanceCreateInfo { vkSType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                               , vkPNext = castPtr callbackInfo
                               , vkFlags = VkInstanceCreateFlags zeroBits
                               , vkPApplicationInfo = appInfo
                               , vkEnabledLayerCount = fromIntegral layerCount
                               , vkPpEnabledLayerNames = layerPtr
                               , vkEnabledExtensionCount = fromIntegral extCount
                               , vkPpEnabledExtensionNames = extPtr
                               } $ \instInfo ->
      alloca $ \instPtr -> vkCreateInstance instInfo nullPtr instPtr >> peek instPtr
   
    findPhysicalDevice inst = (intPtrToPtr . fromIntegral) <$> ivrSystemGetOutputDevice TextureType_Vulkan (castPtr inst)

    createDevice inst phys dexts family = with 1 $ \prioPtr ->
      with VkDeviceQueueCreateInfo { vkSType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                   , vkPNext = nullPtr
                                   , vkFlags = VkDeviceQueueCreateFlags zeroBits
                                   , vkQueueFamilyIndex = family
                                   , vkQueueCount = 1
                                   , vkPQueuePriorities = prioPtr
                                   } $ \queueInfo ->
      withArrayLen dexts $ \extCount extPtr ->
      with VkDeviceCreateInfo { vkSType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
                              , vkPNext = nullPtr
                              , vkFlags = VkDeviceCreateFlags 0
                              , vkQueueCreateInfoCount = 1
                              , vkPQueueCreateInfos = queueInfo
                              , vkEnabledLayerCount = 0
                              , vkPpEnabledLayerNames = nullPtr
                              , vkEnabledExtensionCount = fromIntegral extCount
                              , vkPpEnabledExtensionNames = extPtr
                              , vkPEnabledFeatures = nullPtr
                              } $ \deviceInfo ->
      alloca $ \devicePtr -> vkCreateDevice phys deviceInfo nullPtr devicePtr >> peek devicePtr
    createCommandPool dev family = 
      with VkCommandPoolCreateInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
                                   , vkPNext = nullPtr
                                   , vkFlags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                   , vkQueueFamilyIndex = family
                                   } $ \poolInfo ->
      alloca $ \poolPtr -> vkCreateCommandPool dev poolInfo nullPtr poolPtr >> peek poolPtr

    createCommandBuffer dev pool = 
      with VkCommandBufferAllocateInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
                                       , vkPNext = nullPtr
                                       , vkCommandPool = pool
                                       , vkLevel = VK_COMMAND_BUFFER_LEVEL_PRIMARY
                                       , vkCommandBufferCount = 1
                                       } $ \allocInfo ->
      alloca $ \bufferPtr -> vkAllocateCommandBuffers dev allocInfo bufferPtr >> peek bufferPtr
    createQueue phys dev idx = do
      queue <- alloca $ \queuePtr -> vkGetDeviceQueue dev (fromIntegral idx) 0 queuePtr >> peek queuePtr
      return queue

    createDebugReport inst callbackPtr = do
      with VkDebugReportCallbackCreateInfoEXT { vkSType = VkStructureType 1000011000
                                              , vkPNext = nullPtr
                                              , vkFlags = VK_DEBUG_REPORT_ERROR_BIT_EXT .|. VK_DEBUG_REPORT_WARNING_BIT_EXT
                                              , vkPfnCallback = callbackPtr
                                              , vkPUserData = nullPtr
                                              } $ \callbackInfo ->
        alloca $ \cbPtr -> vkCreateDebugReportCallbackEXT inst callbackInfo nullPtr cbPtr >> peek cbPtr
      


foreign import ccall "wrapper" createDebugCallbackPtr :: (VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> Ptr Void -> IO VkBool32) -> IO PFN_vkDebugReportCallbackEXT
      

-- creates an RGBA image, 8bit/channel
-- no staging buffer
newVulkanImage :: VulkanInfo -> V2 Int -> IO VulkanImage
newVulkanImage info size = do
  (image, imageMemory, imageSize) <- createImage
  sem <- createSemaphore

  mfd <- getMemoryFd imageMemory
  (memObj, tex) <- createOpenGLTexture mfd

  sfd <- getSemaphoreFd sem
  semObj <- createOpenGLSemaphore sfd
  transitionImage image
  return $ VulkanImage image sem imageMemory imageSize tex memObj semObj (VkExtent3D (fromIntegral $ size ^. _x) (fromIntegral $ size ^. _y) 1)
  
  where
    realSize = (size ^. _x) * (size ^. _y) * 4
    findMemory flags bits = do
      props <- alloca $ \propsPtr -> vkGetPhysicalDeviceMemoryProperties (info^.vulkanPhysicalDevice) propsPtr >> peek propsPtr
      -- TODO: make this non-partial
      Just idx <- return $ VF.ifoldr (\idx elem st -> case st of
        Nothing -> if vkPropertyFlags elem .&. flags == flags && ((bits `shiftR` (fromIntegral idx)) .&. 1 == 1)  then Just idx else Nothing
        rem -> rem) Nothing (vkMemoryTypes props)
      return idx
    allocateMemory flags size bits = do
      memoryTypeIndex <- findMemory flags bits
      with VkExportMemoryAllocateInfoKHR { vkSType = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
                                         , vkPNext = nullPtr
                                         , vkHandleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                         } $ \exportInfo ->
        with VkMemoryAllocateInfo { vkSType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                                , vkPNext = castPtr exportInfo
                                , vkAllocationSize = size
                                , vkMemoryTypeIndex = fromIntegral memoryTypeIndex
                                } $ \allocInfo ->
        alloca $ \memoryPtr -> vkAllocateMemory (info^.vulkanDevice) allocInfo nullPtr memoryPtr >> peek memoryPtr

    createImage = do
      image <- with VkImageCreateInfo { vkSType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
                                      , vkPNext = nullPtr
                                      , vkFlags = zeroBits
                                      , vkImageType = VK_IMAGE_TYPE_2D
                                      , vkFormat = VK_FORMAT_R8G8B8A8_UNORM
                                      , vkExtent = VkExtent3D (fromIntegral $ size ^. _x) (fromIntegral $ size ^. _y) 1
                                      , vkMipLevels = 1
                                      , vkArrayLayers = 1
                                      , vkTiling = VK_IMAGE_TILING_OPTIMAL
                                      , vkSamples = VK_SAMPLE_COUNT_1_BIT
                                      , vkUsage = VK_IMAGE_USAGE_TRANSFER_DST_BIT .|. VK_IMAGE_USAGE_TRANSFER_SRC_BIT .|. VK_IMAGE_USAGE_SAMPLED_BIT
                                      , vkSharingMode = VK_SHARING_MODE_EXCLUSIVE
                                      , vkQueueFamilyIndexCount = 0
                                      , vkPQueueFamilyIndices = nullPtr
                                      , vkInitialLayout = VK_IMAGE_LAYOUT_UNDEFINED
                                      } $ \imageInfo ->
        alloca $ \imagePtr -> vkCreateImage (info^.vulkanDevice) imageInfo nullPtr imagePtr  >> peek imagePtr
      (VkMemoryRequirements size _ bits) <- alloca $ \memReqsPtr -> vkGetImageMemoryRequirements (info^.vulkanDevice) image memReqsPtr >> peek memReqsPtr
      memory <- allocateMemory VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT size bits
      vkBindImageMemory (info^.vulkanDevice) image memory (VkDeviceSize 0)
      return (image, memory, size)
      
    transitionImage image = do
      beginCommand info
      with VkImageMemoryBarrier { vkSType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
                                , vkPNext = nullPtr
                                , vkSrcAccessMask = zeroBits
                                , vkDstAccessMask = VK_ACCESS_TRANSFER_READ_BIT
                                , vkOldLayout = VK_IMAGE_LAYOUT_UNDEFINED
                                , vkNewLayout = VK_IMAGE_LAYOUT_GENERAL
                                , vkSrcQueueFamilyIndex = info ^. vulkanQueueFamilyIndex
                                , vkDstQueueFamilyIndex = info ^. vulkanQueueFamilyIndex
                                , vkImage = image 
                                , vkSubresourceRange = VkImageSubresourceRange VK_IMAGE_ASPECT_COLOR_BIT 0 1 0 1
                                } $ \barrier ->
        vkCmdPipelineBarrier (info^.vulkanCommandBuffer) VK_PIPELINE_STAGE_TRANSFER_BIT VK_PIPELINE_STAGE_TRANSFER_BIT zeroBits 0 nullPtr 0 nullPtr 1 barrier
      endCommand info Nothing

    getMemoryFd mem = do
      with VkMemoryGetFdInfoKHR { vkSType = VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
                                , vkPNext = nullPtr
                                , vkMemory = mem
                                , vkHandleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                } $ \getFdInfo ->
        alloca $ \fdPtr -> vkGetMemoryFdKHR (info ^. vulkanDevice) getFdInfo fdPtr >> peek fdPtr

    createOpenGLTexture fd = do
      memObj <- alloca $ \ptr -> glCreateMemoryObjectsEXT 1 ptr >> peek ptr
      glImportMemoryFdEXT memObj (fromIntegral realSize) GL_HANDLE_TYPE_OPAQUE_FD_EXT (fromIntegral fd)
      
      tex <- genObjectName 
      textureBinding Texture2D $= Just tex
      glTexStorageMem2DEXT GL_TEXTURE_2D 1 GL_RGBA8 (fromIntegral $ size ^. _x) (fromIntegral $ size ^. _y) memObj 0
      textureBinding Texture2D $= Nothing
      checkForErrors
      return (memObj, tex)

    createSemaphore = do
      with VkExportSemaphoreCreateInfoKHR { vkSType = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
                                          , vkPNext = nullPtr
                                          , vkHandleTypes = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                          } $ \exportSemInfo ->
        with VkSemaphoreCreateInfo { vkSType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
                                   , vkPNext = castPtr exportSemInfo
                                   , vkFlags = zeroBits
                                   } $ \semInfo ->
        alloca $ \semPtr -> vkCreateSemaphore (info ^. vulkanDevice) semInfo nullPtr semPtr >> peek semPtr

    getSemaphoreFd sem = do
      with VkSemaphoreGetFdInfoKHR { vkSType = VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
                                   , vkPNext = nullPtr
                                   , vkSemaphore = sem
                                   , vkHandleType = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                   } $ \getFdInfo ->
        alloca $ \fdPtr -> vkGetSemaphoreFdKHR (info ^. vulkanDevice) getFdInfo fdPtr >> peek fdPtr

    createOpenGLSemaphore fd = do
      semObj <- alloca $ \ptr -> glGenSemaphoresEXT 1 ptr >> peek ptr
      glImportSemaphoreFdEXT semObj GL_HANDLE_TYPE_OPAQUE_FD_EXT (fromIntegral fd)
      checkForErrors
      return semObj
    
      
        

beginCommand :: VulkanInfo -> IO VkResult
beginCommand info = with VkCommandBufferBeginInfo { vkSType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                                             , vkPNext = nullPtr
                                             , vkFlags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
                                             , vkPInheritanceInfo = nullPtr
                                             } $ \beginInfo -> vkBeginCommandBuffer (info ^. vulkanCommandBuffer) beginInfo

endCommand :: VulkanInfo -> Maybe VkSemaphore -> IO VkResult
endCommand info maybeSem = do
      vkEndCommandBuffer (info ^. vulkanCommandBuffer)
      withMaybeSem $ \(semPtr, maskPtr) ->
        with (info ^. vulkanCommandBuffer) $ \cmdBufferPtr ->
        with VkSubmitInfo { vkSType = VK_STRUCTURE_TYPE_SUBMIT_INFO
                          , vkPNext = nullPtr
                          , vkCommandBufferCount = 1
                          , vkPCommandBuffers = cmdBufferPtr
                          , vkWaitSemaphoreCount = if semPtr == nullPtr then 0 else 1
                          , vkPWaitSemaphores = semPtr
                          , vkPWaitDstStageMask = maskPtr
                          , vkSignalSemaphoreCount = 0
                          , vkPSignalSemaphores = nullPtr
                          } $ \submitInfo ->
        vkQueueSubmit (info^.vulkanQueue) 1 submitInfo (VkFence 0)
      vkQueueWaitIdle (info^.vulkanQueue)
      vkResetCommandBuffer (info^.vulkanCommandBuffer) zeroBits

  where
    withMaybeSem f =
      case maybeSem of
        Just sem -> with sem $ \semPtr
          -> with VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT $ \maskPtr
          -> f (semPtr, maskPtr)
        Nothing -> f (nullPtr, nullPtr)

transitionImage :: VulkanInfo -> VulkanImage
                -> VkImageLayout -> VkImageLayout
                -> VkAccessFlagBits -> VkAccessFlagBits
                -> Bool -- ^ True if we want to wait on the image semaphore
                -> IO ()
transitionImage info image src dst srcMask dstMask waitSem = do
  beginCommand info
  with VkImageMemoryBarrier { vkSType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
                            , vkPNext = nullPtr
                            , vkSrcAccessMask = srcMask
                            , vkDstAccessMask = dstMask
                            , vkOldLayout = src
                            , vkNewLayout = dst
                            , vkSrcQueueFamilyIndex = info ^. vulkanQueueFamilyIndex
                            , vkDstQueueFamilyIndex = info ^. vulkanQueueFamilyIndex
                            , vkImage = image^.imageImage
                            , vkSubresourceRange = VkImageSubresourceRange VK_IMAGE_ASPECT_COLOR_BIT 0 1 0 1
                            } $ \barrier ->
    vkCmdPipelineBarrier (info^.vulkanCommandBuffer) VK_PIPELINE_STAGE_TRANSFER_BIT VK_PIPELINE_STAGE_TRANSFER_BIT zeroBits 0 nullPtr 0 nullPtr 1 barrier
  endCommand info $ if waitSem then Just (image ^. imageSemaphore) else Nothing
  return ()

newViveCompositor :: Bool -> IO ViveCompositor
newViveCompositor verbose = do
  when verbose
      (debugOutput $= Enabled >> debugMessageCallback $= Just print)

  wldp <- wl_display_create
  wcomp <- weston_compositor_create wldp nullPtr

  setup_weston_log_handler
  westonCompositorSetEmptyRuleNames wcomp

  --todo hack; make this into a proper withXXX function
  res <- with (WestonX11BackendConfig (WestonBackendConfig westonX11BackendConfigVersion (sizeOf (undefined :: WestonX11BackendConfig)))
           False
           False
           False) $ weston_compositor_load_backend wcomp WestonBackendX11 . castPtr

  when (res > 0) $ ioError $ userError "Error when loading backend"

  (_, initErr) <- vrInit VRApplication_Scene 
  unless (initErr == VRInitError_None) (error $ show initErr)


  blend $= Disabled
  
  socketName <- wl_display_add_socket_auto wldp
  putStrLn $ "Socket: " ++ socketName
  setEnv "WAYLAND_DISPLAY" socketName

  mainLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionNormal
  bgLayer <- newWestonLayer wcomp
  weston_layer_set_position mainLayer WestonLayerPositionBackground

  seat <- newSimulaSeat

  windowedApi <- weston_windowed_output_get_api wcomp

  info <- newVulkanInfo verbose

  let dpRot = axisAngle (V3 1 0 0) (radians (negate 25))
  let dpTf = translate (V3 0 0.8 1.25) !*! m33_to_m44 (fromQuaternion dpRot)

  -- double the width for viewpoints
  realSize@(width, height) <- ivrSystemGetRecommendedRenderTargetSize
  let recVSize = V2 (fromIntegral $ width * 2) (fromIntegral height)

  rec
    baseCompositor <- BaseCompositor scene display wldp wcomp
                      <$> newMVar M.empty <*> newOpenGlData
                      <*> newMVar Nothing <*> newMVar Nothing
                      <*> pure mainLayer

    scene <- Scene <$> newBaseNode scene Nothing identity
             <*> newMVar 0 <*> newMVar 0
             <*> pure wm <*> newMVar (Some baseCompositor) <*> newMVar [] <*> newMVar Nothing

    let outputPendingSignal = westonCompositorOutputPendingSignal wcomp
    outputPendingPtr <- createNotifyFuncPtr (onOutputPending windowedApi width height baseCompositor)
    addListenerToSignal outputPendingSignal outputPendingPtr

    let outputCreatedSignal = westonCompositorOutputCreatedSignal wcomp
    outputCreatedPtr <- createNotifyFuncPtr (onOutputCreated baseCompositor)
    addListenerToSignal outputCreatedSignal outputCreatedPtr
 
    westonWindowedOutputCreate windowedApi wcomp "X"

    Just glctx <- readMVar (baseCompositor ^. baseCompositorGlContext)
    glCtxMakeCurrent glctx
    image <- newVulkanImage info recVSize
    display <- newDisplay glctx recVSize (V2 0.325 0.1) scene dpTf (Just (image ^. imageTexture))
    wm <- newWindowManager scene seat

  let near = 0.01
      far = 100
      leftParams = V4 0 0 0.5 1
      rightParams = V4 0.5 0 0.5 1
      center = V3 0 0 0
  -- refactor??
  leftEyeTf <- m34_to_m44 <$> ivrSystemGetEyeToHeadTransform Eye_Left
  leftEyeProj <- ivrSystemGetProjectionMatrix Eye_Left near far
  vpLeft <- newViewPoint 0.01 100 display display leftEyeTf leftParams center
  viewPointOverrideProjectionMatrix vpLeft leftEyeProj

  rightEyeTf <- m34_to_m44 <$> ivrSystemGetEyeToHeadTransform Eye_Right
  rightEyeProj <- ivrSystemGetProjectionMatrix Eye_Right near far
  vpRight <- newViewPoint 0.01 100 display display rightEyeTf rightParams center
  viewPointOverrideProjectionMatrix vpRight rightEyeProj

  writeMVar (display ^. displayViewpoints) [vpLeft, vpRight]
  writeMVar (scene ^. sceneDisplays) [display]



  let api = defaultWestonDesktopApi {
        apiSurfaceAdded = onSurfaceCreated baseCompositor,
        apiSurfaceRemoved = onSurfaceDestroyed baseCompositor,
        apiCommitted = onSurfaceCommit baseCompositor
        }

  
  westonDesktopCreate wcomp api nullPtr
  installHandler sigUSR1 Ignore Nothing
  wet_load_xwayland wcomp

  let interface = defaultWestonPointerGrabInterface {
        grabPointerFocus = onPointerFocus baseCompositor,
        grabPointerButton = onPointerButton baseCompositor
        }

  interfacePtr <- new interface
  weston_compositor_set_default_pointer_grab wcomp interfacePtr

  -- hackhack

  viveComp <- ViveCompositor baseCompositor info image
              <$> newMVar mempty
              <*> newMVar mempty
              <*> newMVar mempty
              <*> newMVar False

  -- setup render models
  glCtxMakeCurrent glctx
  forM_ [k_unTrackedDeviceIndex_Hmd + 1 .. k_unMaxTrackedDeviceCount] $ \idx' -> do
    let idx = fromIntegral idx'
    connected <- ivrSystemIsTrackedDeviceConnected idx
    when connected $ setupRenderModel viveComp idx

  return viveComp

  where
    onSurfaceCreated compositor surface  _ = do
      putStrLn "surface created"
      createSurface compositor surface
      return ()


    onSurfaceDestroyed compositor surface _ = do
      --TODO destroy surface in wm
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. baseCompositorSurfaceMap) 
      case simulaSurface of
        Just simulaSurface -> do
          modifyMVar' (compositor ^. baseCompositorSurfaceMap) (M.delete ws)
          let wm = compositor ^. baseCompositorScene.sceneWindowManager
          setSurfaceMapped simulaSurface False
          wmDestroySurface wm simulaSurface
        _ -> return ()
  
    onSurfaceCommit compositor surface x y _ = do
      ws <- weston_desktop_surface_get_surface surface
      simulaSurface <- M.lookup ws <$> readMVar (compositor ^. baseCompositorSurfaceMap)
      case simulaSurface of
        Just simulaSurface -> do
          setSurfaceMapped simulaSurface True
          -- need to figure out surface type
          return ()
        _ -> return ()

    onOutputPending windowedApi width height compositor _ outputPtr = do
      putStrLn "output pending"
      let output = WestonOutput $ castPtr outputPtr
      --TODO hack
      weston_output_set_scale output 1
      weston_output_set_transform output 0
      westonWindowedOutputSetSize windowedApi output (fromIntegral width) (fromIntegral height)

      weston_output_enable output
      return ()


    onOutputCreated compositor _ outputPtr = do
      putStrLn "output created"
      let output = WestonOutput $ castPtr outputPtr
      writeMVar (compositor ^. baseCompositorOutput) $ Just output
      let wc = compositor ^. baseCompositorWestonCompositor
      renderer <- westonCompositorGlRenderer wc
      eglctx <- westonGlRendererContext renderer
      egldp <- westonGlRendererDisplay renderer
      eglsurf <- westonOutputRendererSurface output
      let glctx = SimulaOpenGLContext eglctx egldp eglsurf  (compositor ^. baseCompositorDisplay.displaySize)

      writeMVar (compositor ^. baseCompositorGlContext) (Just glctx)
      return ()


    onPointerFocus compositor grab = do
      pointer <- westonPointerFromGrab grab
      pos' <- westonPointerPosition pointer
      let pos = (`div` 256) <$> pos'
      setFocusForPointer compositor pointer pos

    onPointerButton compositor grab time button state = do
      pointer <- westonPointerFromGrab grab
      pos' <- westonPointerPosition pointer
      let pos = (`div` 256) <$> pos'
      setFocusForPointer compositor pointer pos
      weston_pointer_send_button pointer time button state

viveCompositorRender :: ViveCompositor -> IO ()
viveCompositorRender viveComp = do
  let comp = viveComp ^. viveCompositorBaseCompositor

  surfaceMap <- readMVar (comp ^. baseCompositorSurfaceMap)
  Just glctx <- readMVar (comp ^. baseCompositorGlContext)
  Just output <- readMVar (comp ^. baseCompositorOutput)

  glCtxMakeCurrent glctx
  bindVertexArrayObject $= Just (comp ^. baseCompositorOpenGlData.openGlVAO)

  handleVrInput viveComp
  
  -- set up context

  let surfaces = M.keys surfaceMap
  let scene  = comp ^. baseCompositorScene
  let simDisplay = comp ^. baseCompositorDisplay

  checkForErrors
  time <- getTime Realtime
  scenePrepareForFrame scene time
  checkForErrors
  weston_output_schedule_repaint output

  sceneDrawFrame scene
  checkForErrors

  let tex = comp ^. baseCompositorDisplay.displayScratchColorBufferTexture
  let info = viveComp^.viveCompositorVulkanInfo
  let image = viveComp^.viveCompositorVulkanImage
  let (VkImage handle) = image^.imageImage

--  with (coerce (image^.imageTexture) :: GLuint) $ \texPtr ->
--    with GL_LAYOUT_GENERAL_EXT $ \layoutPtr ->
--    glSignalSemaphoreEXT (image ^. imageOpenGLSemaphoreObject) 0 nullPtr 1 texPtr layoutPtr

  let (VkFormat format) = VK_FORMAT_R8G8B8A8_UNORM
  transitionImage info image VK_IMAGE_LAYOUT_GENERAL VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL VK_ACCESS_TRANSFER_WRITE_BIT VK_ACCESS_TRANSFER_READ_BIT False -- True

  let (VkExtent3D width height _) = image^.imageExtents 


  with (VRVulkanTextureData handle (castPtr $ info^.vulkanDevice) (castPtr $ info^.vulkanPhysicalDevice)
         (castPtr $ info^.vulkanInstance) (castPtr $ info^.vulkanQueue) (info^.vulkanQueueFamilyIndex) width height
         (fromIntegral format) 1) $ \texDataPtr' -> do
    let texDataPtr = castPtr texDataPtr'
    with (OVRTexture texDataPtr TextureType_Vulkan ColorSpace_Auto) $ \txPtr -> do
      err <- with (VRTextureBounds 0 0 0.5 1) $ \boundsPtr ->
        ivrCompositorSubmit Eye_Left txPtr boundsPtr Submit_Default

      when (err /= VRCompositorError_None) $ print err

      err <- with (VRTextureBounds 0.5 0 1 1) $ \boundsPtr ->
        ivrCompositorSubmit Eye_Right txPtr boundsPtr Submit_Default

      when (err /= VRCompositorError_None) $ print err


  transitionImage info image VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL VK_IMAGE_LAYOUT_GENERAL VK_ACCESS_TRANSFER_READ_BIT VK_ACCESS_TRANSFER_WRITE_BIT False

  Some seat <- compositorSeat comp
  pointer <- seatPointer seat
  pos <- readMVar (pointer ^. pointerGlobalPosition)
  drawMousePointer (comp ^. baseCompositorDisplay) (comp ^. baseCompositorOpenGlData.openGlDataMousePointer) pos

  emitOutputFrameSignal output
  eglSwapBuffers (glctx ^. simulaOpenGlContextEglDisplay) (glctx ^. simulaOpenGlContextEglSurface)
  sceneFinishFrame scene


  (err, renderPoses, _) <- ivrCompositorWaitGetPoses
  when (err /= VRCompositorError_None) $ print err

  when ( k_unTrackedDeviceIndex_Hmd < length renderPoses) $ do
    let hmdPose = m34_to_m44 . poseDeviceToAbsoluteTracking $ renderPoses !! k_unTrackedDeviceIndex_Hmd
    setNodeTransform simDisplay hmdPose
    updateVrModelPoses viveComp (map (m34_to_m44 . poseDeviceToAbsoluteTracking) renderPoses)
  
  bindVertexArrayObject $= Nothing

  return ()

--TODO: making a weston pointer here, but the name doesn't match
setupRenderModel :: ViveCompositor -> TrackedDeviceIndex -> IO ()
setupRenderModel viveComp idx = do
  (TrackedProp_Success, rmName) <- ivrSystemGetStringTrackedDeviceProperty idx Prop_RenderModelName_String
  putStr "RENDER MODEL: "
  putStrLn rmName
      
  model <- createRenderModel rmName
  modifyMVar' (viveComp ^. viveCompositorModels) (M.insert idx model)

  devClass <- ivrSystemGetTrackedDeviceClass idx
  case devClass of
    TrackedDeviceClass_Controller -> do
      (seat:_) <- westonCompositorSeats (viveComp ^. viveCompositorBaseCompositor . baseCompositorWestonCompositor)
      pointer <- weston_seat_get_pointer seat
      modifyMVar' (viveComp ^. viveCompositorControllers) (M.insert idx pointer)
    _ -> return ()

  where
    createRenderModel rmName = do
      (model, modelPtr) <- loadRenderModel rmName
      (texture, texturePtr) <- loadRenderModelTexture (modelDiffuseTextureId model)

      let scene = viveComp ^. viveCompositorBaseCompositor.baseCompositorScene
      vrModel <- newSimulaVrModel scene rmName model texture
      
      ivrRenderModelsFreeRenderModel modelPtr
      ivrRenderModelsFreeTexture texturePtr

      return vrModel

    -- refactor (generalize)
    loadRenderModel rmName = do
      (err, ptr) <- ivrRenderModelsLoadRenderModel_Async rmName
      case err of
        VRRenderModelError_Loading -> threadDelay 1000 >> loadRenderModel rmName
        VRRenderModelError_None -> (,ptr) <$> peek ptr 
        _ -> error $ "Failed to load render model: " ++ show err

    loadRenderModelTexture texId = do
      (err, ptr) <- ivrRenderModelsLoadTexture_Async texId
      case err of 
        VRRenderModelError_Loading -> threadDelay 1000 >> loadRenderModelTexture texId
        VRRenderModelError_None -> (,ptr) <$>  peek ptr
        _ -> error $ "Failed to load render model texture: " ++ show err

handleVrInput :: ViveCompositor -> IO ()
handleVrInput viveComp = loop
  where
    processEvent event = case eventType event of
      KnownEvent VREvent_TrackedDeviceActivated -> setupRenderModel viveComp (eventTrackedDeviceIndex event)
      KnownEvent btn
        | isButtonEvent btn
          -> sendButtonPress viveComp (eventTrackedDeviceIndex event) btn (eventData event)

      other -> putStr "EVENT: " >> print other

    loop = do
      maybeEvent <- ivrSystemPollNextEvent
      case maybeEvent of
        Just event -> do
          processEvent event
          loop
        _ -> return ()

    isButtonEvent VREvent_ButtonPress = True
    isButtonEvent VREvent_ButtonUnpress = True
    isButtonEvent _ = False



sendButtonPress :: ViveCompositor -> TrackedDeviceIndex -> EVREventType -> VREvent_Data -> IO ()
sendButtonPress viveComp idx ety edt = do
  models <- readMVar (viveComp ^. viveCompositorModels)
  cts <- readMVar (viveComp ^. viveCompositorControllers)
  case (M.lookup idx models, M.lookup idx cts) of
    (Just model, Just pointer) -> go model pointer
    _ -> return ()

  when (ety == VREvent_ButtonUnpress && isGripButton edt) $ do
     modifyMVar' (viveComp ^. viveCompositorTargetedWindows) (M.delete idx)
     
  where

    isTriggerButton (VREvent_Controller EButton_Axis1) = True
    isTriggerButton (VREvent_Controller EButton_ApplicationMenu) = True
    isTriggerButton _ = False

    isGripButton (VREvent_Controller EButton_Grip) = True
    isGripButton _ = False
    
    go model pointer = do
      tf <- nodeWorldTransform model
      let ray = transformRay (Ray 0 (V3 0 0 (negate 1))) tf
      putStr $ show ety ++ ": "
      print ray

      inter <- nodeIntersectWithSurfaces (viveComp ^. viveCompositorBaseCompositor. baseCompositorScene) ray
      case inter of
        Nothing -> return ()
        Just rsi -> do
          Some node <- return (rsi ^. rsiSurfaceNode)
          let coords = rsi ^. rsiSurfaceCoordinates
          putStr "Intersection coords: "
          print coords

          putStr "Button: "
          let (VREvent_Controller edb) = edt
          print edb

          when (isGripButton edt && ety == VREvent_ButtonPress) $ do
            ntf <- nodeTransform node
            modifyMVar' (viveComp ^. viveCompositorTargetedWindows) (M.insert idx ((tf, ntf), rsi))
            putStrLn $ "targeted something with" ++ show idx

          when (isTriggerButton edt) $ do
            Some seat <- compositorSeat viveComp
            Some surface <- wsnSurface node
            case cast surface of
              Nothing -> return ()
              Just surface -> do
                weston_pointer_set_focus pointer (surface ^. simulaSurfaceView) (truncate (256 * coords ^. _x)) (truncate (256 * coords ^. _y))
                seat <- westonPointerSeat pointer
                kbd <- weston_seat_get_keyboard seat
                ws <- weston_desktop_surface_get_surface $ surface ^. simulaSurfaceWestonDesktopSurface
                weston_keyboard_set_focus kbd ws
  
                time <- getTime Realtime
                let usec = fromIntegral $ toNanoSecs time `div` 1000
                weston_pointer_send_button pointer usec (toWestonButton edt) (toState ety) --see libinput and wayland for enums; converting later
              
    toState VREvent_ButtonPress = 1
    toState VREvent_ButtonUnpress = 0
    toState _ = error "didn't get a button event"

    toWestonButton (VREvent_Controller EButton_Axis1) = 0x110
    toWestonButton (VREvent_Controller EButton_ApplicationMenu) = 0x111
    toWestonButton _ = error "invalid vr button"

-- this is pretty horrible. optimize away from list asap
updateVrModelPoses :: ViveCompositor -> [M44 Float] -> IO ()
updateVrModelPoses viveComp renderPoses = do
  models <- readMVar (viveComp ^. viveCompositorModels)
  targets <- readMVar (viveComp ^. viveCompositorTargetedWindows)
  
  forM_ (M.toList models) $ \(idx, model) -> when (fromIntegral idx < length renderPoses) $ 
    setNodeTransform model (renderPoses !! fromIntegral idx)

  isResizing <- readMVar (viveComp  ^. viveCompositorIsResizing)
  --HACKHACK: this assumes that at most 2 buttons are pressed
  let groups = groupBy ((==) `on` view (_2._2.rsiSurfaceNode) ) $ M.toList targets
  when (not.null $ groups) $ print $ map (map (view _1)) groups
  forM_ groups $ \grp -> case length grp of
    2 -> resizeWindow models grp
    1 | not isResizing -> dragWindow models grp
    _ -> return ()

  when (null groups && isResizing) $ writeMVar (viveComp ^. viveCompositorIsResizing) False

  where
    updatedRay models (idx, (_,rsi)) = case M.lookup idx models of
      Just model -> do
        tf <- nodeWorldTransform model
        return $ transformRay (Ray 0 (V3 0 0 (negate 1))) tf
      Nothing -> return (rsi ^. rsiRay)


    calcDistance r1 t1 r2 t2 = norm (vec2 - vec1)
      where
        vec1 = solveRay r1 t1
        vec2 = solveRay r2 t2

    -- invariant: node is equal for rsi1 and rsi2
    resizeWindow models xs@[(_, ((_, tf), rsi1)), (_, (_, rsi2))] = do
     putStrLn "resizing"
     writeMVar (viveComp ^. viveCompositorIsResizing) True
     Some node <- pure $ rsi1 ^. rsiSurfaceNode
     let dold = calcDistance (rsi1 ^. rsiRay) (rsi1 ^. rsiT) (rsi2 ^. rsiRay) (rsi2 ^. rsiT)
     [ray1n, ray2n] <- mapM (updatedRay models) xs
     let dnew = calcDistance ray1n (rsi1 ^. rsiT) ray2n (rsi2 ^. rsiT)
     let ratio = dnew/dold
     print ratio
     setNodeTransform node (tf !*! (scale (V3 ratio ratio 1)))

    dragWindow models [x@(idx, ((tf, ntf), rsi))] = do
     putStrLn "dragging"
     Some node <- pure $ rsi ^. rsiSurfaceNode
     newTf <- case M.lookup idx models of
       Just model -> nodeWorldTransform model
       Nothing -> return tf

     setNodeTransform node (newTf !*! inv44 tf !*! ntf)
     

instance Compositor ViveCompositor where
  startCompositor viveComp = do
--      fbStatus <- get $ framebufferStatus Framebuffer
--      dfbStatus <- get $ framebufferStatus DrawFramebuffer
--      rfbStatus <- get $ framebufferStatus ReadFramebuffer
--      putStrLn $ "Framebuffer status: " ++ show fbStatus
--      putStrLn $ "Draw framebuffer status: " ++ show dfbStatus
--      putStrLn $ "Read framebuffer status: " ++ show rfbStatus)
    let comp = viveComp ^. viveCompositorBaseCompositor
    let wc = comp ^. baseCompositorWestonCompositor
    oldFunc <- getRepaintOutput wc
    newFunc <- createRendererRepaintOutputFunc (onRender viveComp oldFunc)
    setRepaintOutput wc newFunc
    weston_compositor_wake wc
    putStrLn "Compositor start"

    Just output <- readMVar (comp ^. baseCompositorOutput)
    forkOS $ forever $ weston_output_schedule_repaint output >> threadDelay 1000
    forkIO $ forever $ do
        let scene = comp ^. baseCompositorScene
        diffTime <- liftM2 diffTimeSpec (readMVar $ scene ^. sceneLastTimestamp) (readMVar $ scene ^. sceneCurrentTimestamp)
        let diff = fromIntegral $ toNanoSecs diffTime
        let fps = floor (10^9/diff)
        putStrLn $ "FPS: " ++ show fps
        threadDelay 1000000


    wl_display_run $ comp ^. baseCompositorWlDisplay

    where
      onRender viveComp oldFunc output damage = viveCompositorRender viveComp

  compositorDisplay viveComp = do
    return (viveComp ^. viveCompositorBaseCompositor . baseCompositorDisplay)

  compositorWlDisplay viveComp =
    viveComp ^. viveCompositorBaseCompositor . baseCompositorWlDisplay

  compositorOpenGLContext viveComp = do
    let baseComp = viveComp ^. viveCompositorBaseCompositor
    Just glctx <- readMVar (baseComp ^. baseCompositorGlContext)
    return (Some glctx)

  compositorSeat viveComp = return (viveComp ^. viveCompositorBaseCompositor . baseCompositorScene.sceneWindowManager.windowManagerDefaultSeat)
    
  compositorGetSurfaceFromResource viveComp resource = do
    let comp = (viveComp ^. viveCompositorBaseCompositor)
    ptr <- wlResourceData resource    
    let ws = WestonSurface (castPtr ptr)
    surface <- weston_surface_get_desktop_surface ws
    putStr "resource ptr: "
    print ptr
    simulaSurface <- M.lookup ws <$> readMVar (comp ^. baseCompositorSurfaceMap)
    case simulaSurface of
      Just simulaSurface -> return (Some simulaSurface)
      _ -> do
        simulaSurface <- createSurface comp surface
        return (Some simulaSurface)
