module Plugin.Debug.MonadoHudOpenXRFrameTiming where

import Control.Concurrent.STM.TVar
import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.STM (atomically)
import qualified Data.List as List
import Data.Maybe
import Foreign
import Foreign.C
import System.Directory
import System.FilePath ((</>))
import System.IO.Unsafe

import Plugin.Debug.MonadoHudTypes

debugMonadoHudOpenXRFrameTimingActiveVar :: TVar Bool
debugMonadoHudOpenXRFrameTimingActiveVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE debugMonadoHudOpenXRFrameTimingActiveVar #-}

debugMonadoHudOpenXRFrameTimingFnsVar :: TVar (Maybe OpenXRFrameTimingFns)
debugMonadoHudOpenXRFrameTimingFnsVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugMonadoHudOpenXRFrameTimingFnsVar #-}

foreign import ccall unsafe "simula_dlsym_rtld_default"
  c_simula_dlsym_rtld_default :: CString -> IO (Ptr ())

foreign import ccall unsafe "simula_dlsym_library"
  c_simula_dlsym_library :: CString -> CString -> IO (Ptr ())

foreign import ccall "dynamic"
  mkSimulaOpenXRDebugMonadoHudSetActive :: FunPtr CFunctionTypeSimulaOpenXRDebugMonadoHudSetActive -> CFunctionTypeSimulaOpenXRDebugMonadoHudSetActive

foreign import ccall "dynamic"
  mkSimulaOpenXRDebugMonadoHudReadSamples :: FunPtr CFunctionTypeSimulaOpenXRDebugMonadoHudReadSamples -> CFunctionTypeSimulaOpenXRDebugMonadoHudReadSamples

openXRLibraryName :: String
openXRLibraryName = "libgodot_openxr.so"

openXRSetActiveSymbol :: String
openXRSetActiveSymbol = "simula_openxr_debug_monado_hud_set_active"

openXRReadSamplesSymbol :: String
openXRReadSamplesSymbol = "simula_openxr_debug_monado_hud_read_samples"

setOpenXRFrameTimingBridgeActive :: Bool -> IO Bool
setOpenXRFrameTimingBridgeActive active = do
  changed <- atomically $ do
    previous <- readTVar debugMonadoHudOpenXRFrameTimingActiveVar
    if previous == active
      then return False
      else do
        writeTVar debugMonadoHudOpenXRFrameTimingActiveVar active
        writeTVar debugMonadoHudOpenXRFrameTimingFnsVar Nothing
        return True
  when changed $
    sendOpenXRFrameTimingActiveFlag active
  return changed

drainDebugMonadoHudOpenXRFrameTimingSamples :: IO [OpenXRFrameTimingSample]
drainDebugMonadoHudOpenXRFrameTimingSamples =
  openXRFrameTimingDrainSamples <$> drainDebugMonadoHudOpenXRFrameTiming

drainDebugMonadoHudOpenXRFrameTiming :: IO OpenXRFrameTimingDrain
drainDebugMonadoHudOpenXRFrameTiming = do
  active <- readTVarIO debugMonadoHudOpenXRFrameTimingActiveVar
  if not active
    then return $ emptyOpenXRFrameTimingDrain False False []
    else do
      maybeFns <- resolveOpenXRFrameTimingFns
      case maybeFns of
        Nothing -> return $ emptyOpenXRFrameTimingDrain True False []
        Just fns -> do
          -- Reasserting while active handles the case where godot-openxr was loaded
          -- after the Haskell plugin first selected the Monado HUD. Use the same
          -- resolved function table for activation and draining so multiple loaded
          -- libgodot_openxr instances cannot split set_active/read_samples.
          openXRFrameTimingSetActive fns 1
          allocaArray debugMonadoHudMaxTimingSamples $ \samplePtr -> do
            count <- openXRFrameTimingReadSamples fns samplePtr (fromIntegral debugMonadoHudMaxTimingSamples)
            samples <- peekArray (fromIntegral count) samplePtr
            return $ emptyOpenXRFrameTimingDrain True True (fmap cOpenXRFrameTimingToHaskell samples)

emptyOpenXRFrameTimingDrain :: Bool -> Bool -> [OpenXRFrameTimingSample] -> OpenXRFrameTimingDrain
emptyOpenXRFrameTimingDrain active fnsFound samples =
  OpenXRFrameTimingDrain
    { openXRFrameTimingDrainActive = active
    , openXRFrameTimingDrainFnsFound = fnsFound
    , openXRFrameTimingDrainSampleCount = length samples
    , openXRFrameTimingDrainSamples = samples
    }

sendOpenXRFrameTimingActiveFlag :: Bool -> IO ()
sendOpenXRFrameTimingActiveFlag active = do
  maybeFns <- resolveOpenXRFrameTimingFns
  forM_ maybeFns $ \fns ->
    openXRFrameTimingSetActive fns (if active then 1 else 0)

resolveOpenXRFrameTimingFns :: IO (Maybe OpenXRFrameTimingFns)
resolveOpenXRFrameTimingFns = do
  cached <- readTVarIO debugMonadoHudOpenXRFrameTimingFnsVar
  case cached of
    Just _ -> return cached
    Nothing -> do
      maybeKnownLibraryFns <- lookupOpenXRFrameTimingFnsInKnownLibraries
      result <-
        case maybeKnownLibraryFns of
          Just _ -> return maybeKnownLibraryFns
          Nothing -> lookupOpenXRFrameTimingFnsInDefault
      forM_ result $ \fns ->
        atomically $ writeTVar debugMonadoHudOpenXRFrameTimingFnsVar (Just fns)
      return result

lookupOpenXRFrameTimingFnsInDefault :: IO (Maybe OpenXRFrameTimingFns)
lookupOpenXRFrameTimingFnsInDefault =
  withCString openXRSetActiveSymbol $ \cSetActiveSymbol ->
    withCString openXRReadSamplesSymbol $ \cReadSamplesSymbol -> do
      setActivePtr <- c_simula_dlsym_rtld_default cSetActiveSymbol
      readSamplesPtr <- c_simula_dlsym_rtld_default cReadSamplesSymbol
      return $ openXRFrameTimingFnsFromPtrs setActivePtr readSamplesPtr

lookupOpenXRFrameTimingFnsInKnownLibraries :: IO (Maybe OpenXRFrameTimingFns)
lookupOpenXRFrameTimingFnsInKnownLibraries = do
  cwd <- getCurrentDirectory
  loadedLibraryPaths <- getLoadedOpenXRLibraryPaths
  let projectLibraryPaths =
        [ cwd </> "addons" </> "godot-openxr" </> "bin" </> "linux" </> openXRLibraryName
        , "addons" </> "godot-openxr" </> "bin" </> "linux" </> openXRLibraryName
        , "./addons" </> "godot-openxr" </> "bin" </> "linux" </> openXRLibraryName
        ]
  firstJustM lookupOpenXRFrameTimingFnsInLibrary $
    List.nub $
      projectLibraryPaths ++ loadedLibraryPaths

getLoadedOpenXRLibraryPaths :: IO [FilePath]
getLoadedOpenXRLibraryPaths = do
  result <-
    ( try $ do
        contents <- readFile "/proc/self/maps"
        length contents `seq` return contents
    ) :: IO (Either SomeException String)
  return $
    case result of
      Left _ -> []
      Right contents ->
        List.nub $
          mapMaybe loadedOpenXRLibraryPathFromMapsLine (lines contents)

loadedOpenXRLibraryPathFromMapsLine :: String -> Maybe FilePath
loadedOpenXRLibraryPathFromMapsLine line =
  case dropWhile (/= '/') line of
    "" -> Nothing
    path
      | openXRLibraryName `List.isInfixOf` path ->
          Just $ stripDeletedSuffix path
      | otherwise ->
          Nothing

stripDeletedSuffix :: FilePath -> FilePath
stripDeletedSuffix path =
  stripSuffixCompat " (deleted)" path

stripSuffixCompat :: String -> String -> String
stripSuffixCompat suffix text
  | suffix `List.isSuffixOf` text =
      take (length text - length suffix) text
  | otherwise =
      text

lookupOpenXRFrameTimingFnsInLibrary :: FilePath -> IO (Maybe OpenXRFrameTimingFns)
lookupOpenXRFrameTimingFnsInLibrary libraryPath =
  withCString libraryPath $ \cLibraryPath ->
    withCString openXRSetActiveSymbol $ \cSetActiveSymbol ->
      withCString openXRReadSamplesSymbol $ \cReadSamplesSymbol -> do
        setActivePtr <- c_simula_dlsym_library cLibraryPath cSetActiveSymbol
        readSamplesPtr <- c_simula_dlsym_library cLibraryPath cReadSamplesSymbol
        return $ openXRFrameTimingFnsFromPtrs setActivePtr readSamplesPtr

openXRFrameTimingFnsFromPtrs :: Ptr () -> Ptr () -> Maybe OpenXRFrameTimingFns
openXRFrameTimingFnsFromPtrs setActivePtr readSamplesPtr =
  if setActivePtr == nullPtr || readSamplesPtr == nullPtr
    then Nothing
    else
      Just
        OpenXRFrameTimingFns
          { openXRFrameTimingSetActive = mkSimulaOpenXRDebugMonadoHudSetActive (castPtrToFunPtr setActivePtr)
          , openXRFrameTimingReadSamples = mkSimulaOpenXRDebugMonadoHudReadSamples (castPtrToFunPtr readSamplesPtr)
          }

firstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
firstJustM _ [] = return Nothing
firstJustM action (x : xs) = do
  result <- action x
  case result of
    Just _ -> return result
    Nothing -> firstJustM action xs

cOpenXRFrameTimingToHaskell :: COpenXRFrameTimingSample -> OpenXRFrameTimingSample
cOpenXRFrameTimingToHaskell sample =
  OpenXRFrameTimingSample
    { openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs = realToFrac (cOpenXRFrameTimingGodotFrameStartToXrWaitFrameMs sample)
    , openXRFrameTimingSampleXrWaitFrameSleepMs = realToFrac (cOpenXRFrameTimingXrWaitFrameMs sample)
    }
