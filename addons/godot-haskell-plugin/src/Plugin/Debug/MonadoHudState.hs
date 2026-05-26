module Plugin.Debug.MonadoHudState where

import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM (atomically)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Time.Clock
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO.Unsafe

import Plugin.Debug.MonadoHudParse
import Plugin.Debug.MonadoHudGC (setDebugMonadoHudGhcGCFrameTimingActive)
import Plugin.Debug.MonadoHudOpenXRFrameTiming (setOpenXRFrameTimingBridgeActive)
import Plugin.Debug.MonadoHudTypes

debugMonadoHudStateVar :: TVar MonadoHudState
debugMonadoHudStateVar = unsafePerformIO $ newTVarIO emptyMonadoHudState
{-# NOINLINE debugMonadoHudStateVar #-}

debugMonadoHudSnapshotCacheVar :: TVar (Maybe (UTCTime, DebugMonadoHudSnapshot))
debugMonadoHudSnapshotCacheVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugMonadoHudSnapshotCacheVar #-}

debugMonadoHudOutputLastWriteVar :: TVar (Maybe UTCTime)
debugMonadoHudOutputLastWriteVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugMonadoHudOutputLastWriteVar #-}

debugMonadoHudOutputInFlightVar :: TVar Bool
debugMonadoHudOutputInFlightVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE debugMonadoHudOutputInFlightVar #-}

-- Fallback order:
-- 1. $SIMULA_MONADO_HUD_LOG
-- 2. $SIMULA_LOG_DIR/monado.log
-- 3. $XDG_CACHE_HOME/Simula/monado.log
getDebugMonadoHudLogPath :: IO FilePath
getDebugMonadoHudLogPath = do
  override <- lookupEnv "SIMULA_MONADO_HUD_LOG"
  case override >>= normalizeEnvString of
    Just path -> return path
    Nothing -> do
      simulaLogDir <- lookupEnv "SIMULA_LOG_DIR"
      xdgCacheHome <- lookupEnv "XDG_CACHE_HOME"
      let fallbackCacheHome = fromMaybe "./.cache" xdgCacheHome
      pickFirstExisting
        [ fmap (</> "monado.log") (simulaLogDir >>= normalizeEnvString)
        , Just (fallbackCacheHome </> "Simula" </> "monado.log")
        ]

getDebugMonadoHudLivePath :: IO FilePath
getDebugMonadoHudLivePath = do
  simulaLogDir <- lookupEnv "SIMULA_LOG_DIR"
  return $
    case simulaLogDir >>= normalizeEnvString of
      Just dir -> dir </> "HUD_monado_live.txt"
      Nothing -> "./HUD_monado_live.txt"

pickFirstExisting :: [Maybe FilePath] -> IO FilePath
pickFirstExisting candidates = do
  let paths = catMaybes candidates
  existing <- filterM doesFileExist paths
  return $
    case existing of
      path : _ -> path
      [] ->
        case paths of
          path : _ -> path
          [] -> "./.cache/Simula/monado.log"

normalizeEnvString :: String -> Maybe String
normalizeEnvString value =
  let trimmed = trim value
   in if List.null trimmed
        then Nothing
        else Just trimmed
  where
    trim =
      List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

setDebugMonadoHudOpenXRFrameTimingActive :: Bool -> IO ()
setDebugMonadoHudOpenXRFrameTimingActive active = do
  changed <- setOpenXRFrameTimingBridgeActive active
  when changed $ do
    atomically $ do
        modifyTVar' debugMonadoHudStateVar clearMonadoHudOpenXRFrameTimingState
        writeTVar debugMonadoHudSnapshotCacheVar Nothing
    setDebugMonadoHudGhcGCFrameTimingActive active

processMonadoHudChunk :: UTCTime -> FilePath -> Integer -> Bool -> String -> MonadoHudState -> MonadoHudState
processMonadoHudChunk now path newOffset resetLog chunk state =
  trimMonadoHudState now $
    (List.foldl' (consumeMonadoHudLine now) baseState completeLines)
      { monadoHudStateLogPath = Just path
      , monadoHudStateLogOffset = newOffset
      , monadoHudStatePartialLine = partialLine
      }
  where
    resetState =
      emptyMonadoHudState
        { monadoHudStateErrors = monadoHudStateErrors state
        }
    baseBeforeLines =
      if resetLog
        then resetState
        else state
    combinedText = monadoHudStatePartialLine baseBeforeLines ++ chunk
    (partialLine, completeLines) = splitCompleteLines combinedText
    baseState = baseBeforeLines { monadoHudStateLogPath = Just path }

recordMonadoHudError :: String -> MonadoHudState -> MonadoHudState
recordMonadoHudError err state =
  state
    { monadoHudStateErrors =
        List.take 12 $
          err : List.filter (/= err) (monadoHudStateErrors state)
    }

trimMonadoHudState :: UTCTime -> MonadoHudState -> MonadoHudState
trimMonadoHudState now state =
  state
    { monadoHudStateFrameSkipEvents =
        trimSeqToLast debugMonadoHudMaxEvents $
          Seq.dropWhileL (\event -> diffUTCTime now (monadoHudFrameSkipEventTime event) > debugMonadoHudWindowInSeconds) (monadoHudStateFrameSkipEvents state)
    , monadoHudStateAppTimingSamples =
        trimSeqToLast debugMonadoHudMaxTimingSamples $
          Seq.dropWhileL (\sample -> diffUTCTime now (monadoTimedAt sample) > debugMonadoHudWindowInSeconds) (monadoHudStateAppTimingSamples state)
    , monadoHudStateCompositorTimingSamples =
        trimSeqToLast debugMonadoHudMaxTimingSamples $
          Seq.dropWhileL (\sample -> diffUTCTime now (monadoTimedAt sample) > debugMonadoHudWindowInSeconds) (monadoHudStateCompositorTimingSamples state)
    , monadoHudStateOpenXRFrameTimingSamples =
        trimSeqToLast debugMonadoHudMaxTimingSamples $
          Seq.dropWhileL (\sample -> diffUTCTime now (monadoTimedAt sample) > debugMonadoHudWindowInSeconds) (monadoHudStateOpenXRFrameTimingSamples state)
    , monadoHudStateGhcGCFrameTimingSamples =
        trimSeqToLast debugMonadoHudMaxTimingSamples $
          Seq.dropWhileL (\sample -> diffUTCTime now (monadoTimedAt sample) > debugMonadoHudWindowInSeconds) (monadoHudStateGhcGCFrameTimingSamples state)
    }

trimSeqToLast :: Int -> Seq.Seq a -> Seq.Seq a
trimSeqToLast maxCount values =
  Seq.drop (max 0 (Seq.length values - maxCount)) values

consumeMonadoHudLine :: UTCTime -> MonadoHudState -> String -> MonadoHudState
consumeMonadoHudLine now state line =
  updateMonadoCompositorTiming now line $
    updateMonadoAppTiming now line $
      addMonadoEventFromLine now line state

addMonadoEventFromLine :: UTCTime -> String -> MonadoHudState -> MonadoHudState
addMonadoEventFromLine now line state
  | Just (kind, ms) <- parseMonadoFrameSkipEventLine line =
      appendEvent kind ms now line state
  | otherwise =
      state

appendEvent :: MonadoHudFrameSkipEventKind -> Maybe Double -> UTCTime -> String -> MonadoHudState -> MonadoHudState
appendEvent kind ms now line state =
  updateEventSessionWorst kind ms $
    state
      { monadoHudStateFrameSkipEvents =
          monadoHudStateFrameSkipEvents state
            |> MonadoHudFrameSkipEvent
                { monadoHudFrameSkipEventTime = now
                , monadoHudFrameSkipEventKind = kind
                , monadoHudFrameSkipEventMs = ms
                , monadoHudFrameSkipEventLine = List.take 240 line
                }
      }

updateEventSessionWorst :: MonadoHudFrameSkipEventKind -> Maybe Double -> MonadoHudState -> MonadoHudState
updateEventSessionWorst MonadoHudDisplayMiss ms state =
  state
    { monadoHudStateWorstDisplayMissSessionMs =
        updateSessionMax (monadoHudStateWorstDisplayMissSessionMs state) ms
    }
updateEventSessionWorst _ _ state = state

updateMonadoAppTiming :: UTCTime -> String -> MonadoHudState -> MonadoHudState
updateMonadoAppTiming now line state =
  case pending1 >>= completePendingAppTiming of
    Just timing ->
      recordMonadoHudAppTimingSession timing $
        state1
          { monadoHudStatePendingAppTiming = Nothing
          , monadoHudStateAppTimingSamples =
              monadoHudStateAppTimingSamples state1
                |> MonadoTimed now timing
          }
    Nothing ->
      state1 { monadoHudStatePendingAppTiming = pending1 }
  where
    state0 =
      case parseDeliveredFrameLine line of
        Nothing -> state
        Just pending -> state { monadoHudStatePendingAppTiming = Just pending }
    pending0 = monadoHudStatePendingAppTiming state0
    pending1 = fmap (`updatePendingAppTimingFromLine` line) pending0
    state1 = state0

recordMonadoHudAppTimingSession :: MonadoAppTiming -> MonadoHudState -> MonadoHudState
recordMonadoHudAppTimingSession timing state =
  state
    { monadoHudStateWorstAppObservedSessionMs =
        updateSessionMax (monadoHudStateWorstAppObservedSessionMs state) (Just $ monadoAppObservedTotalMs timing)
    , monadoHudStateWorstAppEstimateSessionMs =
        updateSessionMax (monadoHudStateWorstAppEstimateSessionMs state) (Just $ monadoAppEstimatedTotalMs timing)
    , monadoHudStateWorstAppCpuEstimateSessionMs =
        updateSessionMax (monadoHudStateWorstAppCpuEstimateSessionMs state) (Just $ monadoAppCpuEstimateMs timing)
    , monadoHudStateWorstAppCpuObservedSessionMs =
        updateSessionMax (monadoHudStateWorstAppCpuObservedSessionMs state) (Just $ monadoAppCpuObservedMs timing)
    , monadoHudStateWorstAppDrawEstimateSessionMs =
        updateSessionMax (monadoHudStateWorstAppDrawEstimateSessionMs state) (Just $ monadoAppDrawEstimateMs timing)
    , monadoHudStateWorstAppDrawObservedSessionMs =
        updateSessionMax (monadoHudStateWorstAppDrawObservedSessionMs state) (Just $ monadoAppDrawObservedMs timing)
    , monadoHudStateWorstAppGpuEstimateSessionMs =
        updateSessionMax (monadoHudStateWorstAppGpuEstimateSessionMs state) (Just $ monadoAppGpuEstimateMs timing)
    , monadoHudStateWorstAppGpuObservedSessionMs =
        updateSessionMax (monadoHudStateWorstAppGpuObservedSessionMs state) (Just $ monadoAppGpuObservedMs timing)
    }

updateMonadoCompositorTiming :: UTCTime -> String -> MonadoHudState -> MonadoHudState
updateMonadoCompositorTiming now line state =
  case pending1 >>= completePendingCompositorTiming of
    Just timing ->
      recordMonadoHudCompositorTimingSession timing $
        state1
          { monadoHudStatePendingCompositorTiming = Nothing
          , monadoHudStateCompositorTimingSamples =
              monadoHudStateCompositorTimingSamples state1
                |> MonadoTimed now timing
          }
    Nothing ->
      state1 { monadoHudStatePendingCompositorTiming = pending1 }
  where
    startsNewBlock = startsNewMonadoCompositorTimingBlock line
    startsBlock = startsMonadoCompositorTimingBlock line
    state0 =
      if startsNewBlock || (startsBlock && isNothing (monadoHudStatePendingCompositorTiming state))
        then state { monadoHudStatePendingCompositorTiming = Just emptyPendingMonadoCompositorTiming }
        else state
    pending0 = monadoHudStatePendingCompositorTiming state0
    pending1 = fmap (`updatePendingCompositorTimingFromLine` line) pending0
    state1 = state0

recordMonadoHudCompositorTimingSession :: MonadoCompositorTiming -> MonadoHudState -> MonadoHudState
recordMonadoHudCompositorTimingSession timing state =
  state
    { monadoHudStateWorstCompositorSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorSessionMs state) (monadoCompositorWakeToSubmitMs timing)
    , monadoHudStateWorstCompositorSubmitToInferredGpuDoneSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorSubmitToInferredGpuDoneSessionMs state) (monadoCompositorSubmitToInferredGpuDoneMs timing)
    , monadoHudStateWorstCompositorInferredGpuDoneToDesiredSessionMs =
        updateSessionMin (monadoHudStateWorstCompositorInferredGpuDoneToDesiredSessionMs state) (monadoCompositorInferredGpuDoneToDesiredMs timing)
    , monadoHudStateWorstCompositorSubmitToActualPresentSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorSubmitToActualPresentSessionMs state) (monadoCompositorSubmitToActualPresentMs timing)
    , monadoHudStateWorstCompositorWakeToActualPresentSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorWakeToActualPresentSessionMs state) (monadoCompositorWakeToActualPresentMs timing)
    , monadoHudStateWorstCompositorDesiredToActualPresentSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorDesiredToActualPresentSessionMs state) (monadoCompositorDesiredToActualPresentMs timing)
    , monadoHudStateWorstCompositorEarliestToActualPresentSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorEarliestToActualPresentSessionMs state) (monadoCompositorEarliestToActualPresentMs timing)
    , monadoHudStateWorstCompositorInfoAfterActualPresentSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorInfoAfterActualPresentSessionMs state) (monadoCompositorInfoAfterActualPresentMs timing)
    , monadoHudStateWorstPresentMarginSessionMs =
        updateSessionMin (monadoHudStateWorstPresentMarginSessionMs state) (monadoCompositorPresentMarginMs timing)
    , monadoHudStateWorstCompositorSinceLastFrameSessionMs =
        updateSessionMax (monadoHudStateWorstCompositorSinceLastFrameSessionMs state) (monadoCompositorSinceLastFrameMs timing)
    }

recordMonadoHudOpenXRFrameTimingSamples :: UTCTime -> [OpenXRFrameTimingSample] -> MonadoHudState -> MonadoHudState
recordMonadoHudOpenXRFrameTimingSamples now timings state =
  trimMonadoHudState now $
    List.foldl' recordOpenXRFrameTimingSample state timings
  where
    recordOpenXRFrameTimingSample current timing =
      recordMonadoHudOpenXRFrameTimingSampleSession timing $
        current
          { monadoHudStateOpenXRFrameTimingSamples =
              monadoHudStateOpenXRFrameTimingSamples current
                |> MonadoTimed now timing
          }

recordMonadoHudOpenXRFrameTimingSampleSession :: OpenXRFrameTimingSample -> MonadoHudState -> MonadoHudState
recordMonadoHudOpenXRFrameTimingSampleSession timing state =
  state
    { monadoHudStateWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs =
        updateSessionMax
          (monadoHudStateWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs state)
          (Just $ openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs timing)
    , monadoHudStateWorstOpenXRXrWaitFrameSleepSessionMs =
        updateSessionMax
          (monadoHudStateWorstOpenXRXrWaitFrameSleepSessionMs state)
          (Just $ openXRFrameTimingSampleXrWaitFrameSleepMs timing)
    }

recordMonadoHudGhcGCFrameTimingSamples :: UTCTime -> [GhcGCFrameTimingSample] -> MonadoHudState -> MonadoHudState
recordMonadoHudGhcGCFrameTimingSamples now timings state =
  trimMonadoHudState now $
    List.foldl' recordGhcGCTiming state timings
  where
    recordGhcGCTiming current timing =
      recordMonadoHudGhcGCFrameTimingSession timing $
        current
          { monadoHudStateGhcGCFrameTimingSamples =
              monadoHudStateGhcGCFrameTimingSamples current
                |> MonadoTimed now timing
          }

recordMonadoHudGhcGCFrameTimingSession :: GhcGCFrameTimingSample -> MonadoHudState -> MonadoHudState
recordMonadoHudGhcGCFrameTimingSession timing state =
  state
    { monadoHudStateWorstGhcGCTimeSessionMs =
        updateSessionMax
          (monadoHudStateWorstGhcGCTimeSessionMs state)
          (Just $ ghcGCFrameTimingSampleGcMs timing)
    }

recordMonadoHudSamplerStatus :: Bool -> Int -> Int -> MonadoHudState -> MonadoHudState
recordMonadoHudSamplerStatus gcStatsEnabled openXRDrainCount gcDrainCount state =
  state
    { monadoHudStateGhcGCStatsEnabled = Just gcStatsEnabled
    , monadoHudStateLastOpenXRFrameTimingDrainCount = openXRDrainCount
    , monadoHudStateLastGhcGCFrameTimingDrainCount = gcDrainCount
    }

clearMonadoHudOpenXRFrameTimingState :: MonadoHudState -> MonadoHudState
clearMonadoHudOpenXRFrameTimingState state =
  state
    { monadoHudStateOpenXRFrameTimingSamples = Seq.empty
    , monadoHudStateGhcGCFrameTimingSamples = Seq.empty
    , monadoHudStateLastOpenXRFrameTimingDrainCount = 0
    , monadoHudStateLastGhcGCFrameTimingDrainCount = 0
    , monadoHudStateWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs = Nothing
    , monadoHudStateWorstOpenXRXrWaitFrameSleepSessionMs = Nothing
    , monadoHudStateWorstGhcGCTimeSessionMs = Nothing
    }

splitCompleteLines :: String -> (String, [String])
splitCompleteLines "" = ("", [])
splitCompleteLines text
  | List.last text == '\n' = ("", lines text)
  | otherwise =
      case lines text of
        [] -> (text, [])
        parsedLines -> (List.last parsedLines, init parsedLines)

updateSessionMax :: Maybe Double -> Maybe Double -> Maybe Double
updateSessionMax current candidate =
  maxMaybe $ catMaybes [current, candidate]

updateSessionMin :: Maybe Double -> Maybe Double -> Maybe Double
updateSessionMin current candidate =
  minMaybe $ catMaybes [current, candidate]

maxMaybe :: [Double] -> Maybe Double
maxMaybe [] = Nothing
maxMaybe values = Just $ List.maximum values

minMaybe :: [Double] -> Maybe Double
minMaybe [] = Nothing
minMaybe values = Just $ List.minimum values
