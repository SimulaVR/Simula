module Plugin.Debug.HudTypes where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import System.Environment (lookupEnv)
import System.IO.Unsafe

debugHudTabBarHeight :: Int
debugHudTabBarHeight = 52

debugHudBaseMessageHeight :: Int
debugHudBaseMessageHeight = 186

debugHudPaddingPixels :: Int
debugHudPaddingPixels = 8

debugHudCloseButtonSize :: Float
debugHudCloseButtonSize = 32

debugHudTabInset :: Float
debugHudTabInset = 6

debugHudTabGap :: Float
debugHudTabGap = 3

data DebugHudMode
  = DebugHudProfile
  | DebugHudMonado
  | DebugHudMemory
  | DebugHudSurfaceBoundaries
  | DebugHudSurfaceCreations
  | DebugHudMouseEvents
  | DebugHudKeyboardEvents
  | DebugHudDepthFirstSurfaces
  | DebugHudDamagedRegions
  deriving (Bounded, Enum, Eq, Ord, Show)

data DebugHudRuntimeState = DebugHudRuntimeState
  { debugHudRuntimeVisible :: Bool
  , debugHudRuntimeActiveMode :: DebugHudMode
  }
  deriving (Eq, Show)

data DebugHudControl
  = DebugHudControlTab DebugHudMode
  | DebugHudControlClose
  deriving (Eq, Show)



debugHudModes :: [DebugHudMode]
debugHudModes = [minBound .. maxBound]

debugHudModeLabel :: DebugHudMode -> String
debugHudModeLabel DebugHudProfile = "Profile"
debugHudModeLabel DebugHudMonado = "Monado\nFrame Timings"
debugHudModeLabel DebugHudMemory = "Memory"
debugHudModeLabel DebugHudSurfaceBoundaries = "Surface Boundaries"
debugHudModeLabel DebugHudSurfaceCreations = "Surface Creations"
debugHudModeLabel DebugHudMouseEvents = "Mouse Events"
debugHudModeLabel DebugHudKeyboardEvents = "Keyboard Events"
debugHudModeLabel DebugHudDepthFirstSurfaces = "Depth First Surfaces"
debugHudModeLabel DebugHudDamagedRegions = "Damaged Regions"

debugHudInitialVisible :: Bool
debugHudInitialVisible = unsafePerformIO $ do
  exactFlagValues <-
    mapM
      lookupEnv
      [ "SIMULA_DEBUG_PROFILE_HUD"
      , "SIMULA_DEBUG_MONADO_HUD"
      , "SIMULA_DEBUG_MEMORY_HUD"
      , "SIMULA_DEBUG_SURFACE_BOUNDARIES"
      , "SIMULA_DEBUG_SURFACE_CREATIONS"
      , "SIMULA_DEBUG_MOUSE_EVENTS"
      , "SIMULA_DEBUG_KEYBOARD_EVENTS"
      , "SIMULA_DEBUG_DEPTH_FIRST_THUMBNAILS"
      ]
  damagedRegionsValue <- lookupEnv "SIMULA_DEBUG_DAMAGED_REGIONS"
  let exactFlagEnabled = any (== Just "1") exactFlagValues
  let damagedRegionsEnabled =
        case damagedRegionsValue of
          Just "" -> False
          Just "0" -> False
          Just _ -> True
          Nothing -> False
  return (exactFlagEnabled || damagedRegionsEnabled)
{-# NOINLINE debugHudInitialVisible #-}

debugHudInitialActiveMode :: DebugHudMode
debugHudInitialActiveMode = unsafePerformIO $ do
  profileValue <- lookupEnv "SIMULA_DEBUG_PROFILE_HUD"
  monadoValue <- lookupEnv "SIMULA_DEBUG_MONADO_HUD"
  memoryValue <- lookupEnv "SIMULA_DEBUG_MEMORY_HUD"
  damagedRegionsValue <- lookupEnv "SIMULA_DEBUG_DAMAGED_REGIONS"
  return $
    case () of
      _
        | profileValue == Just "1" -> DebugHudProfile
        | monadoValue == Just "1" -> DebugHudMonado
        | memoryValue == Just "1" -> DebugHudMemory
        | damagedRegionsValueEnabled damagedRegionsValue -> DebugHudDamagedRegions
        | otherwise -> DebugHudProfile
  where
    damagedRegionsValueEnabled value =
      case value of
        Just "" -> False
        Just "0" -> False
        Just _ -> True
        Nothing -> False
{-# NOINLINE debugHudInitialActiveMode #-}

debugHudInitialState :: DebugHudRuntimeState
debugHudInitialState =
  DebugHudRuntimeState
    { debugHudRuntimeVisible = debugHudInitialVisible
    , debugHudRuntimeActiveMode = debugHudInitialActiveMode
    }

debugHudRuntimeStateVar :: TVar DebugHudRuntimeState
debugHudRuntimeStateVar = unsafePerformIO $ newTVarIO debugHudInitialState
{-# NOINLINE debugHudRuntimeStateVar #-}

getDebugHudRuntimeState :: IO DebugHudRuntimeState
getDebugHudRuntimeState =
  readTVarIO debugHudRuntimeStateVar

debugHudVisible :: IO Bool
debugHudVisible =
  debugHudRuntimeVisible <$> getDebugHudRuntimeState

debugHudActiveMode :: IO DebugHudMode
debugHudActiveMode =
  debugHudRuntimeActiveMode <$> getDebugHudRuntimeState

debugHudModeActive :: DebugHudMode -> IO Bool
debugHudModeActive mode = do
  state <- getDebugHudRuntimeState
  return $
    debugHudRuntimeVisible state
      && debugHudRuntimeActiveMode state == mode

debugHudSetMode :: DebugHudMode -> IO ()
debugHudSetMode mode =
  atomically $
    modifyTVar' debugHudRuntimeStateVar $
      \state ->
        state
          { debugHudRuntimeVisible = True
          , debugHudRuntimeActiveMode = mode
          }

debugHudClose :: IO ()
debugHudClose =
  atomically $
    modifyTVar' debugHudRuntimeStateVar $
      \state -> state { debugHudRuntimeVisible = False }

debugHudToggleVisible :: IO ()
debugHudToggleVisible =
  atomically $
    modifyTVar' debugHudRuntimeStateVar $
      \state -> state { debugHudRuntimeVisible = not (debugHudRuntimeVisible state) }
