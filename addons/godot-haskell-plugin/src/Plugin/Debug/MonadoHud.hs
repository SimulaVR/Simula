{-# LANGUAGE TypeApplications #-}

module Plugin.Debug.MonadoHud where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket, finally)
import Control.Exception.Safe (SomeException, try)
import Control.Monad
import Control.Monad.STM
import Data.Colour
import Data.Colour.SRGB.Linear
import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import Data.Maybe
import Data.Time.Clock
import Godot.Core.GodotVisualServer as G
import qualified Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import qualified Godot.Methods as G
import Linear
import Numeric (showFFloat)
import System.Directory
import System.IO

import Plugin.Debug.MonadoHudSnapshot
import Plugin.Debug.MonadoHudState
import Plugin.Debug.MonadoHudGC
import Plugin.Debug.MonadoHudOpenXRFrameTiming
import Plugin.Debug.MonadoHudTypes
import Plugin.Imports
import Plugin.Types

data MonadoHudCellTone
  = MonadoHudToneNeutral
  | MonadoHudToneEstimate
  | MonadoHudToneActual
  | MonadoHudToneDeltaAhead
  | MonadoHudToneDeltaMiss
  | MonadoHudToneHot
  | MonadoHudToneWarn

data MonadoHudCellSegment = MonadoHudCellSegment
  { monadoHudCellSegmentTone :: MonadoHudCellTone
  , monadoHudCellSegmentText :: String
  }

data MonadoHudRow = MonadoHudRow
  { monadoHudRowMetric       :: String
  , monadoHudRowCount        :: String
  , monadoHudRowRate         :: String
  , monadoHudRowLatest       :: [MonadoHudCellSegment]
  , monadoHudRowAverage      :: [MonadoHudCellSegment]
  , monadoHudRowWorstWindow  :: [MonadoHudCellSegment]
  , monadoHudRowWorstSession :: [MonadoHudCellSegment]
  , monadoHudRowHot          :: Bool
  , monadoHudRowWarn         :: Bool
  }

getDebugMonadoHudSnapshot :: IO DebugMonadoHudSnapshot
getDebugMonadoHudSnapshot = do
  path <- getDebugMonadoHudLogPath
  setDebugMonadoHudOpenXRFrameTimingActive True
  now <- getCurrentTime
  cached <- readTVarIO debugMonadoHudSnapshotCacheVar
  case cached of
    Just (cachedAt, snapshot)
      | diffUTCTime now cachedAt < debugMonadoHudSnapshotIntervalInSeconds ->
          return snapshot
    _ -> do
      refreshDebugMonadoHudState now path
      state <- readTVarIO debugMonadoHudStateVar
      let snapshot = buildDebugMonadoHudSnapshot now path state
      atomically $ writeTVar debugMonadoHudSnapshotCacheVar (Just (now, snapshot))
      queueDebugMonadoHudOutput snapshot
      return snapshot

refreshDebugMonadoHudState :: UTCTime -> FilePath -> IO ()
refreshDebugMonadoHudState now path = do
  exists <- doesFileExist path
  if not exists
    then
      atomically $
        modifyTVar' debugMonadoHudStateVar $
          recordMonadoHudError ("waiting for Monado log: " ++ path)
    else do
      state <- readTVarIO debugMonadoHudStateVar
      readResult <- try (readMonadoLogChunk path state) :: IO (Either SomeException (Integer, Bool, String))
      case readResult of
        Left err ->
          atomically $
            modifyTVar' debugMonadoHudStateVar $
              recordMonadoHudError ("failed reading Monado log: " ++ show err)
        Right (newOffset, resetLog, chunk) ->
          atomically $
            modifyTVar' debugMonadoHudStateVar $
              processMonadoHudChunk now path newOffset resetLog chunk
  openXRDrain <- drainDebugMonadoHudOpenXRFrameTiming
  let openXRFrameTimingSamples = openXRFrameTimingDrainSamples openXRDrain
  when (openXRFrameTimingDrainActive openXRDrain && not (openXRFrameTimingDrainFnsFound openXRDrain)) $
    atomically $
      modifyTVar' debugMonadoHudStateVar $
        recordMonadoHudError "OpenXR timing symbols not found; rebuild/restart godot-openxr for pre-openxr/xrWaitFrame/GC rows"
  when (openXRFrameTimingDrainActive openXRDrain && openXRFrameTimingDrainFnsFound openXRDrain && List.null openXRFrameTimingSamples) $
    atomically $
      modifyTVar' debugMonadoHudStateVar $
        recordMonadoHudError "OpenXR timing bridge found but produced 0 samples; process_openxr may not be running, may return before xrWaitFrame, or may be in another libgodot_openxr instance"
  gcStatsEnabled <- ghcGCFrameTimingStatsEnabled
  when (not gcStatsEnabled && not (List.null openXRFrameTimingSamples)) $
    atomically $
      modifyTVar' debugMonadoHudStateVar $
        recordMonadoHudError "GHC RTS stats are disabled; Monado GC rows need RTS -T at startup"
  gcTimings <- drainDebugMonadoHudGhcGCFrameTimingSamples openXRFrameTimingSamples
  atomically $
    modifyTVar' debugMonadoHudStateVar $
      recordMonadoHudSamplerStatus
        gcStatsEnabled
        (List.length openXRFrameTimingSamples)
        (List.length gcTimings)
  unless (List.null openXRFrameTimingSamples) $
    atomically $
      modifyTVar' debugMonadoHudStateVar $
        recordMonadoHudOpenXRFrameTimingSamples now openXRFrameTimingSamples
  unless (List.null gcTimings) $
    atomically $
      modifyTVar' debugMonadoHudStateVar $
        recordMonadoHudGhcGCFrameTimingSamples now gcTimings

readMonadoLogChunk :: FilePath -> MonadoHudState -> IO (Integer, Bool, String)
readMonadoLogChunk path state =
  withFile path ReadMode $ \handle -> do
    size <- hFileSize handle
    let pathChanged = monadoHudStateLogPath state /= Just path
    let fileTruncated = size < monadoHudStateLogOffset state
    let resetLog = pathChanged || fileTruncated
    let offset = if resetLog then 0 else monadoHudStateLogOffset state
    hSeek handle AbsoluteSeek offset
    bytes <- B.hGetContents handle
    return (size, resetLog, B.unpack bytes)

queueDebugMonadoHudOutput :: DebugMonadoHudSnapshot -> IO ()
queueDebugMonadoHudOutput snapshot = do
  shouldQueue <- atomically $ do
    inFlight <- readTVar debugMonadoHudOutputInFlightVar
    lastWrite <- readTVar debugMonadoHudOutputLastWriteVar
    let now = debugMonadoHudSnapshotUpdatedAt snapshot
    let oldEnough =
          case lastWrite of
            Nothing -> True
            Just lastWriteAt -> diffUTCTime now lastWriteAt >= debugMonadoHudOutputIntervalInSeconds
    if inFlight || not oldEnough
      then return False
      else do
        writeTVar debugMonadoHudOutputInFlightVar True
        writeTVar debugMonadoHudOutputLastWriteVar (Just now)
        return True
  when shouldQueue $
    void $
      forkIO $
        writeDebugMonadoHudOutput snapshot
          `finally` atomically (writeTVar debugMonadoHudOutputInFlightVar False)

writeDebugMonadoHudOutput :: DebugMonadoHudSnapshot -> IO ()
writeDebugMonadoHudOutput snapshot = do
  path <- getDebugMonadoHudLivePath
  _ <- try (writeFile path (formatDebugMonadoHudLiveSnapshot snapshot)) :: IO (Either SomeException ())
  return ()

formatDebugMonadoHudLiveSnapshot :: DebugMonadoHudSnapshot -> String
formatDebugMonadoHudLiveSnapshot snapshot =
  List.unlines $
    [ "SIMULA_DEBUG_MONADO_HUD live"
    , "updated: " ++ show (debugMonadoHudSnapshotUpdatedAt snapshot)
    , "log: " ++ debugMonadoHudSnapshotLogPath snapshot
    , "window_s: " ++ formatSecondsValue (debugMonadoHudSnapshotWindowSeconds snapshot)
    , "compositor_probably_missed_frame_warnings: " ++ show (debugMonadoHudSnapshotDisplayMissCount snapshot)
    , "stale_late_frames_dropped: " ++ show (debugMonadoHudSnapshotOldFrameDropCount snapshot)
    , "frames_missed: " ++ show (debugMonadoHudSnapshotPacingMissCount snapshot)
    , "prediction_skips: " ++ show (debugMonadoHudSnapshotPredictionSkipCount snapshot)
    , "openxr_frame_timing_samples_window: " ++ show (debugMonadoHudSnapshotOpenXRFrameTimingSampleCount snapshot)
    , "openxr_frame_timing_samples_latest_drain: " ++ show (debugMonadoHudSnapshotLastOpenXRFrameTimingDrainCount snapshot)
    , "gc_timing_samples_window: " ++ show (debugMonadoHudSnapshotGhcGCFrameTimingSampleCount snapshot)
    , "gc_timing_samples_latest_drain: " ++ show (debugMonadoHudSnapshotLastGhcGCFrameTimingDrainCount snapshot)
    , "ghc_rts_stats_enabled: " ++ formatMaybeBool (debugMonadoHudSnapshotGhcGCStatsEnabled snapshot)
    , "latest_compositor_probably_missed_frame_warning_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotLatestDisplayMissMs snapshot)
    , "worst_window_compositor_probably_missed_frame_warning_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstDisplayMissWindowMs snapshot)
    , "worst_session_compositor_probably_missed_frame_warning_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstDisplayMissSessionMs snapshot)
    , "latest_pre_xr_godot_work_ms: " ++ formatMaybeMs (openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs <$> debugMonadoHudSnapshotLatestOpenXRFrameTimingSample snapshot)
    , "worst_window_pre_xr_godot_work_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameWindowMs snapshot)
    , "worst_session_pre_xr_godot_work_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs snapshot)
    , "avg_pre_xr_godot_work_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgOpenXRGodotFrameStartToXrWaitFrameMs snapshot)
    , "latest_pre_xr_gc_ms: " ++ formatMaybeMs (ghcGCFrameTimingSampleGcMs <$> debugMonadoHudSnapshotLatestGhcGCFrameTimingSample snapshot)
    , "worst_window_pre_xr_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstGhcGCTimeWindowMs snapshot)
    , "worst_session_pre_xr_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstGhcGCTimeSessionMs snapshot)
    , "avg_pre_xr_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgGhcGCTimeMs snapshot)
    , "latest_pre_xr_non_gc_ms: " ++ formatMaybeMs (ghcGCFrameTimingSampleNonGCGodotFrameStartToXrWaitFrameMs <$> debugMonadoHudSnapshotLatestGhcGCFrameTimingSample snapshot)
    , "worst_window_pre_xr_non_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstNonGCGodotFrameStartToXrWaitFrameWindowMs snapshot)
    , "worst_session_pre_xr_non_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstNonGCGodotFrameStartToXrWaitFrameSessionMs snapshot)
    , "avg_pre_xr_non_gc_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgNonGCGodotFrameStartToXrWaitFrameMs snapshot)
    , "latest_xrWaitFrame_wait_ms: " ++ formatMaybeMs (openXRFrameTimingSampleXrWaitFrameSleepMs <$> debugMonadoHudSnapshotLatestOpenXRFrameTimingSample snapshot)
    , "worst_window_xrWaitFrame_wait_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstOpenXRXrWaitFrameSleepWindowMs snapshot)
    , "worst_session_xrWaitFrame_wait_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstOpenXRXrWaitFrameSleepSessionMs snapshot)
    , "avg_xrWaitFrame_wait_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgOpenXRXrWaitFrameSleepMs snapshot)
    , "latest_app_observed_ms: " ++ formatMaybeMs (monadoAppObservedTotalMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "worst_window_app_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppObservedWindowMs snapshot)
    , "worst_session_app_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppObservedSessionMs snapshot)
    , "latest_app_estimate_ms: " ++ formatMaybeMs (monadoAppEstimatedTotalMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "worst_window_app_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppEstimateWindowMs snapshot)
    , "worst_session_app_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppEstimateSessionMs snapshot)
    , "latest_app_cpu_observed_ms: " ++ formatMaybeMs (monadoAppCpuObservedMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "latest_app_cpu_estimate_ms: " ++ formatMaybeMs (monadoAppCpuEstimateMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "worst_window_app_cpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppCpuObservedWindowMs snapshot)
    , "worst_window_app_cpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppCpuEstimateWindowMs snapshot)
    , "worst_session_app_cpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppCpuObservedSessionMs snapshot)
    , "worst_session_app_cpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppCpuEstimateSessionMs snapshot)
    , "latest_app_draw_observed_ms: " ++ formatMaybeMs (monadoAppDrawObservedMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "latest_app_draw_estimate_ms: " ++ formatMaybeMs (monadoAppDrawEstimateMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "worst_window_app_draw_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppDrawObservedWindowMs snapshot)
    , "worst_window_app_draw_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppDrawEstimateWindowMs snapshot)
    , "worst_session_app_draw_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppDrawObservedSessionMs snapshot)
    , "worst_session_app_draw_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppDrawEstimateSessionMs snapshot)
    , "latest_app_gpu_observed_ms: " ++ formatMaybeMs (monadoAppGpuObservedMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "latest_app_gpu_estimate_ms: " ++ formatMaybeMs (monadoAppGpuEstimateMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    , "worst_window_app_gpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppGpuObservedWindowMs snapshot)
    , "worst_window_app_gpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppGpuEstimateWindowMs snapshot)
    , "worst_session_app_gpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppGpuObservedSessionMs snapshot)
    , "worst_session_app_gpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstAppGpuEstimateSessionMs snapshot)
    , "avg_app_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppObservedMs snapshot)
    , "avg_app_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppEstimateMs snapshot)
    , "avg_app_cpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppCpuObservedMs snapshot)
    , "avg_app_cpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppCpuEstimateMs snapshot)
    , "avg_app_draw_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppDrawObservedMs snapshot)
    , "avg_app_draw_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppDrawEstimateMs snapshot)
    , "avg_app_gpu_observed_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppGpuObservedMs snapshot)
    , "avg_app_gpu_estimate_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgAppGpuEstimateMs snapshot)
    , "latest_compositor_wake_to_submit_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotLatestCompositorTiming snapshot >>= monadoCompositorWakeToSubmitMs)
    , "worst_window_compositor_wake_to_submit_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorWindowMs snapshot)
    , "worst_session_compositor_wake_to_submit_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSessionMs snapshot)
    , "latest_present_margin_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotLatestCompositorTiming snapshot >>= monadoCompositorPresentMarginMs)
    , "worst_window_present_margin_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstPresentMarginWindowMs snapshot)
    , "worst_session_present_margin_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstPresentMarginSessionMs snapshot)
    , "avg_compositor_wake_to_submit_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorMs snapshot)
    , "latest_est_app_plus_comp_submit_ms: " ++ formatMaybeMs latestEstimatedAppSubmit
    , "latest_est_app_plus_comp_actual_scanout_ms: " ++ formatMaybeMs latestEstimatedAppActualPresent
    , "estimated_scanout_to_visible_ms: " ++ formatMsValue debugMonadoHudPresentToDisplayOffsetMs ++ "ms"
    , "latest_est_app_plus_comp_visible_ms: " ++ formatMaybeMs latestEstimatedAppVisible
    , "latest_comp_submit_to_inferred_gpu_done_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorSubmitToInferredGpuDoneMs)
    , "worst_window_comp_submit_to_inferred_gpu_done_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneWindowMs snapshot)
    , "worst_session_comp_submit_to_inferred_gpu_done_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneSessionMs snapshot)
    , "avg_comp_submit_to_inferred_gpu_done_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorSubmitToInferredGpuDoneMs snapshot)
    , "latest_comp_inferred_gpu_done_to_desired_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorInferredGpuDoneToDesiredMs)
    , "worst_window_comp_inferred_gpu_done_to_desired_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredWindowMs snapshot)
    , "worst_session_comp_inferred_gpu_done_to_desired_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredSessionMs snapshot)
    , "avg_comp_inferred_gpu_done_to_desired_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorInferredGpuDoneToDesiredMs snapshot)
    , "latest_comp_submit_to_actual_scanout_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorSubmitToActualPresentMs)
    , "worst_window_comp_submit_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentWindowMs snapshot)
    , "worst_session_comp_submit_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentSessionMs snapshot)
    , "avg_comp_submit_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorSubmitToActualPresentMs snapshot)
    , "latest_comp_wake_to_actual_scanout_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorWakeToActualPresentMs)
    , "worst_window_comp_wake_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorWakeToActualPresentWindowMs snapshot)
    , "worst_session_comp_wake_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorWakeToActualPresentSessionMs snapshot)
    , "avg_comp_wake_to_actual_scanout_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorWakeToActualPresentMs snapshot)
    , "latest_actual_minus_desired_present_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorDesiredToActualPresentMs)
    , "worst_window_actual_minus_desired_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentWindowMs snapshot)
    , "worst_session_actual_minus_desired_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentSessionMs snapshot)
    , "avg_actual_minus_desired_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorDesiredToActualPresentMs snapshot)
    , "latest_actual_minus_earliest_present_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorEarliestToActualPresentMs)
    , "worst_window_actual_minus_earliest_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentWindowMs snapshot)
    , "worst_session_actual_minus_earliest_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentSessionMs snapshot)
    , "avg_actual_minus_earliest_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorEarliestToActualPresentMs snapshot)
    , "latest_info_after_actual_present_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorInfoAfterActualPresentMs)
    , "worst_window_info_after_actual_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentWindowMs snapshot)
    , "worst_session_info_after_actual_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentSessionMs snapshot)
    , "avg_info_after_actual_present_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorInfoAfterActualPresentMs snapshot)
    , "avg_present_margin_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgPresentMarginMs snapshot)
    , "latest_comp_since_last_frame_ms: " ++ formatMaybeMs (latestCompositorTiming >>= monadoCompositorSinceLastFrameMs)
    , "worst_window_comp_since_last_frame_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSinceLastFrameWindowMs snapshot)
    , "worst_session_comp_since_last_frame_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotWorstCompositorSinceLastFrameSessionMs snapshot)
    , "avg_comp_since_last_frame_ms: " ++ formatMaybeMs (debugMonadoHudSnapshotAvgCompositorSinceLastFrameMs snapshot)
    , ""
    , padRight 26 "event" ++ padLeft 10 "window" ++ padLeft 10 "/min" ++ padLeft 12 "latest_ms" ++ padLeft 15 "worst_window" ++ padLeft 16 "worst_session"
    , List.replicate 89 '-'
    , eventLine "compositor_probably_missed_frame_warning" (debugMonadoHudSnapshotDisplayMissCount snapshot) (debugMonadoHudSnapshotLatestDisplayMissMs snapshot) (debugMonadoHudSnapshotWorstDisplayMissWindowMs snapshot) (debugMonadoHudSnapshotWorstDisplayMissSessionMs snapshot)
    , eventLine "stale_late_frame_dropped" (debugMonadoHudSnapshotOldFrameDropCount snapshot) Nothing Nothing Nothing
    , eventLine "frame_missed" (debugMonadoHudSnapshotPacingMissCount snapshot) Nothing Nothing Nothing
    , eventLine "prediction_skip" (debugMonadoHudSnapshotPredictionSkipCount snapshot) Nothing Nothing Nothing
    ]
      ++ formatLatestAppLines snapshot
      ++ formatLatestCompositorLines snapshot
      ++ formatErrors snapshot
  where
    latestAppTiming = debugMonadoHudSnapshotLatestAppTiming snapshot
    latestCompositorTiming = debugMonadoHudSnapshotLatestCompositorTiming snapshot
    latestEstimatedAppSubmit = do
      appTiming <- latestAppTiming
      compositorTiming <- latestCompositorTiming
      monadoEstimatedAppCompositorSubmitMs appTiming compositorTiming
    latestEstimatedAppActualPresent = do
      appTiming <- latestAppTiming
      compositorTiming <- latestCompositorTiming
      monadoEstimatedAppCompositorActualPresentMs appTiming compositorTiming
    latestEstimatedAppVisible = do
      appTiming <- latestAppTiming
      compositorTiming <- latestCompositorTiming
      monadoEstimatedAppCompositorVisibleMs appTiming compositorTiming

    eventLine label count latest worstWindow worstSession =
      padRight 26 label
        ++ padLeft 10 (show count)
        ++ padLeft 10 (formatMsValue $ ratePerMinute count)
        ++ padLeft 12 (formatMaybeMs latest)
        ++ padLeft 15 (formatMaybeMs worstWindow)
        ++ padLeft 16 (formatMaybeMs worstSession)

formatLatestAppLines :: DebugMonadoHudSnapshot -> [String]
formatLatestAppLines snapshot =
  case debugMonadoHudSnapshotLatestAppTiming snapshot of
    Nothing -> []
    Just timing ->
      [ ""
      , "Latest app timing from U_PACING_APP_LOG:"
      , padRight 12 "component" ++ padLeft 12 "estimate" ++ padLeft 12 "observed"
      , List.replicate 36 '-'
      , componentLine "cpu" (monadoAppCpuEstimateMs timing) (monadoAppCpuObservedMs timing)
      , componentLine "draw" (monadoAppDrawEstimateMs timing) (monadoAppDrawObservedMs timing)
      , componentLine "gpu" (monadoAppGpuEstimateMs timing) (monadoAppGpuObservedMs timing)
      , componentLine "total" (monadoAppEstimatedTotalMs timing) (monadoAppObservedTotalMs timing)
      ]
  where
    componentLine label estimate observed =
      padRight 12 label
        ++ padLeft 12 (formatMsValue estimate)
        ++ padLeft 12 (formatMsValue observed)

formatLatestCompositorLines :: DebugMonadoHudSnapshot -> [String]
formatLatestCompositorLines snapshot =
  case debugMonadoHudSnapshotLatestCompositorTiming snapshot of
    Nothing -> []
    Just timing ->
      [ ""
      , "Latest compositor timing from U_PACING_COMPOSITOR_LOG:"
      , "wake_to_submit_ms: " ++ formatMaybeMs (monadoCompositorWakeToSubmitMs timing)
      , "submit_to_inferred_gpu_done_ms: " ++ formatMaybeMs (monadoCompositorSubmitToInferredGpuDoneMs timing)
      , "inferred_gpu_done_to_desired_present_ms: " ++ formatMaybeMs (monadoCompositorInferredGpuDoneToDesiredMs timing)
      , "submit_to_actual_scanout_ms: " ++ formatMaybeMs (monadoCompositorSubmitToActualPresentMs timing)
      , "wake_to_actual_scanout_ms: " ++ formatMaybeMs (monadoCompositorWakeToActualPresentMs timing)
      , "actual_minus_desired_present_ms: " ++ formatMaybeMs (monadoCompositorDesiredToActualPresentMs timing)
      , "actual_minus_earliest_present_ms: " ++ formatMaybeMs (monadoCompositorEarliestToActualPresentMs timing)
      , "info_after_actual_present_ms: " ++ formatMaybeMs (monadoCompositorInfoAfterActualPresentMs timing)
      , "present_margin_ms: " ++ formatMaybeMs (monadoCompositorPresentMarginMs timing)
      , "since_last_frame_ms: " ++ formatMaybeMs (monadoCompositorSinceLastFrameMs timing)
      ]

formatErrors :: DebugMonadoHudSnapshot -> [String]
formatErrors snapshot =
  case debugMonadoHudSnapshotErrors snapshot of
    [] -> []
    errors -> "" : "Errors:" : errors

drawDebugHudMonado :: CanvasBase -> DebugMonadoHudSnapshot -> GodotDynamicFont -> Float -> Float -> Float -> Float -> IO ()
drawDebugHudMonado cb snapshot debugFont left top availableWidth availableHeight = do
  debugPutStrLn "Plugin.Debug.MonadoHud.drawDebugHudMonado"
  G.set_size debugFont 16
  let fontAscent = debugMonadoHudFontAscentPixels
  let lineStep = debugMonadoHudLineStepPixels
  let stripTop = top + lineStep + 3
  let stripHeight = 12
  let tableTop = stripTop + stripHeight + lineStep + 4
  let metricRight = left + availableWidth * 0.20
  let countRight = left + availableWidth * 0.31
  let rateRight = left + availableWidth * 0.39
  let latestRight = left + availableWidth * 0.55
  let avgRight = left + availableWidth * 0.70
  let worstWindowRight = left + availableWidth * 0.85
  let worstSessionRight = left + availableWidth
  let metricWidth = max 1 (metricRight - left - 8)
  let countWidth = max 1 (countRight - metricRight - 8)
  let rateWidth = max 1 (rateRight - countRight - 8)
  let latestWidth = max 1 (latestRight - rateRight - 8)
  let avgWidth = max 1 (avgRight - latestRight - 8)
  let worstWindowWidth = max 1 (worstWindowRight - avgRight - 8)
  let worstSessionWidth = max 1 (worstSessionRight - worstWindowRight - 8)
  headerColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
  neutralColor <- (toLowLevel $ (rgb 0.74 0.78 0.82) `withOpacity` 1.0) :: IO GodotColor
  estimateColor <- (toLowLevel $ (rgb 1.0 0.58 0.12) `withOpacity` 1.0) :: IO GodotColor
  hotColor <- (toLowLevel $ (rgb 1.0 0.42 0.24) `withOpacity` 1.0) :: IO GodotColor
  warnColor <- (toLowLevel $ (rgb 1.0 0.78 0.25) `withOpacity` 1.0) :: IO GodotColor
  aheadColor <- (toLowLevel $ (rgb 0.30 0.92 0.43) `withOpacity` 1.0) :: IO GodotColor
  lineColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.18) :: IO GodotColor
  let headerBaseline = top + fontAscent
  drawText (formatMonadoHudHeader snapshot) headerColor left headerBaseline availableWidth
  drawEventStrip stripTop stripHeight
  drawBoldText "metric" headerColor left tableTop metricWidth
  drawRightAlignedBoldText "count (window)" headerColor countRight tableTop countWidth
  drawRightAlignedBoldText "count/min" headerColor rateRight tableTop rateWidth
  drawRightAlignedBoldText "latest" headerColor latestRight tableTop latestWidth
  drawRightAlignedBoldText "avg" headerColor avgRight tableTop avgWidth
  drawRightAlignedBoldText "worst (window)" headerColor worstWindowRight tableTop worstWindowWidth
  drawRightAlignedBoldText "worst (session)" headerColor worstSessionRight tableTop worstSessionWidth
  drawHorizontalLine lineColor (tableTop + 5)
  drawRows
    neutralColor
    headerColor
    estimateColor
    hotColor
    warnColor
    aheadColor
    lineColor
    lineStep
    metricWidth
    countRight
    rateRight
    latestRight
    avgRight
    worstWindowRight
    worstSessionRight
    countWidth
    rateWidth
    latestWidth
    avgWidth
    worstWindowWidth
    worstSessionWidth
    (tableTop + lineStep)
  where
    drawRows
      neutralColor
      actualColor
      estimateColor
      hotColor
      warnColor
      aheadColor
      lineColor
      lineStep
      metricWidth
      countRight
      rateRight
      latestRight
      avgRight
      worstWindowRight
      worstSessionRight
      countWidth
      rateWidth
      latestWidth
      avgWidth
      worstWindowWidth
      worstSessionWidth
      firstBaseline = do
        let appTiming = debugMonadoHudSnapshotLatestAppTiming snapshot
        let compTiming = debugMonadoHudSnapshotLatestCompositorTiming snapshot
        let openXRTiming = debugMonadoHudSnapshotLatestOpenXRFrameTimingSample snapshot
        let latestPreXRActual = openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs <$> openXRTiming
        let latestXrWaitFrameSleepActual = openXRFrameTimingSampleXrWaitFrameSleepMs <$> openXRTiming
        let latestAppEstimate = monadoAppEstimatedTotalMs <$> appTiming
        let latestAppActual = monadoAppObservedTotalMs <$> appTiming
        let latestCompositorCpuActual = compTiming >>= monadoCompositorWakeToSubmitMs
        let latestCompositorGpuActual = compTiming >>= monadoCompositorSubmitToInferredGpuDoneMs
        let latestCompositorActual = (+) <$> latestCompositorCpuActual <*> latestCompositorGpuActual
        let latestMarginActual = compTiming >>= monadoCompositorPresentMarginMs
        let latestEstimatedMargin = compTiming >>= monadoCompositorInferredGpuDoneToDesiredMs
        let latestMarginForWarn =
              case latestEstimatedMargin of
                Just _ -> latestEstimatedMargin
                Nothing -> latestMarginActual
        let sumMaybes values = sum <$> sequence values
        let latestKnownWithoutSleepActual = sumMaybes [latestPreXRActual, latestAppActual, latestCompositorActual]
        let latestTotalActual =
              sumMaybes
                [ latestPreXRActual
                , latestXrWaitFrameSleepActual
                , latestAppActual
                , latestCompositorActual
                , latestMarginActual
                ]
        let unknown = Nothing :: Maybe Double
        let avgCompositorActual =
              (+)
                <$> debugMonadoHudSnapshotAvgCompositorMs snapshot
                <*> debugMonadoHudSnapshotAvgCompositorSubmitToInferredGpuDoneMs snapshot
        let worstWindowCompositorActual =
              (+)
                <$> debugMonadoHudSnapshotWorstCompositorWindowMs snapshot
                <*> debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneWindowMs snapshot
        let worstSessionCompositorActual =
              (+)
                <$> debugMonadoHudSnapshotWorstCompositorSessionMs snapshot
                <*> debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneSessionMs snapshot
        let avgKnownWithoutSleepActual =
              sumMaybes
                [ debugMonadoHudSnapshotAvgOpenXRGodotFrameStartToXrWaitFrameMs snapshot
                , debugMonadoHudSnapshotAvgAppObservedMs snapshot
                , avgCompositorActual
                ]
        let avgTotalActual =
              sumMaybes
                [ debugMonadoHudSnapshotAvgOpenXRGodotFrameStartToXrWaitFrameMs snapshot
                , debugMonadoHudSnapshotAvgOpenXRXrWaitFrameSleepMs snapshot
                , debugMonadoHudSnapshotAvgAppObservedMs snapshot
                , avgCompositorActual
                , debugMonadoHudSnapshotAvgPresentMarginMs snapshot
                ]
        let worstWindowKnownWithoutSleepActual =
              sumMaybes
                [ debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameWindowMs snapshot
                , debugMonadoHudSnapshotWorstAppObservedWindowMs snapshot
                , worstWindowCompositorActual
                ]
        let worstSessionKnownWithoutSleepActual =
              sumMaybes
                [ debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs snapshot
                , debugMonadoHudSnapshotWorstAppObservedSessionMs snapshot
                , worstSessionCompositorActual
                ]
        let eventRow label count latest worstWindow worstSession =
              MonadoHudRow
                { monadoHudRowMetric = label
                , monadoHudRowCount = show count
                , monadoHudRowRate = formatMsValueOne $ ratePerMinute count
                , monadoHudRowLatest = plainMonadoHudCell (if count > 0 then MonadoHudToneHot else MonadoHudToneNeutral) (formatMaybeMsOne latest)
                , monadoHudRowAverage = []
                , monadoHudRowWorstWindow = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstWindow)
                , monadoHudRowWorstSession = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstSession)
                , monadoHudRowHot = count > 0
                , monadoHudRowWarn = False
                }
        let timingRow label latestEstimate latestActual avgEstimate avgActual worstWindowEstimate worstWindowActual worstSessionEstimate worstSessionActual warn =
              MonadoHudRow
                { monadoHudRowMetric = label
                , monadoHudRowCount = ""
                , monadoHudRowRate = ""
                , monadoHudRowLatest = monadoHudEstimateActualDeltaCell latestEstimate latestActual
                , monadoHudRowAverage = monadoHudEstimateActualDeltaCell avgEstimate avgActual
                , monadoHudRowWorstWindow = monadoHudEstimateActualDeltaCell worstWindowEstimate worstWindowActual
                , monadoHudRowWorstSession = monadoHudEstimateActualDeltaCell worstSessionEstimate worstSessionActual
                , monadoHudRowHot = False
                , monadoHudRowWarn = warn
                }
        let actualOnlyTimingRow label sampleCount latestActual avgActual worstWindowActual worstSessionActual warn =
              MonadoHudRow
                { monadoHudRowMetric = label
                , monadoHudRowCount = show sampleCount
                , monadoHudRowRate = ""
                , monadoHudRowLatest = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne latestActual)
                , monadoHudRowAverage = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne avgActual)
                , monadoHudRowWorstWindow = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstWindowActual)
                , monadoHudRowWorstSession = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstSessionActual)
                , monadoHudRowHot = False
                , monadoHudRowWarn = warn
                }
        let actualOnlyTimingRowWithLatestCell label sampleCount latestCell avgActual worstWindowActual worstSessionActual warn =
              MonadoHudRow
                { monadoHudRowMetric = label
                , monadoHudRowCount = show sampleCount
                , monadoHudRowRate = ""
                , monadoHudRowLatest = latestCell
                , monadoHudRowAverage = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne avgActual)
                , monadoHudRowWorstWindow = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstWindowActual)
                , monadoHudRowWorstSession = plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne worstSessionActual)
                , monadoHudRowHot = False
                , monadoHudRowWarn = warn
                }
        let missingGCLatestCell latestActual =
              case latestActual of
                Just _ ->
                  plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne latestActual)
                Nothing
                  | debugMonadoHudSnapshotOpenXRFrameTimingSampleCount snapshot == 0 ->
                      plainMonadoHudCell MonadoHudToneWarn "no OpenXR"
                  | debugMonadoHudSnapshotGhcGCStatsEnabled snapshot == Just False ->
                      plainMonadoHudCell MonadoHudToneWarn "-T off"
                  | otherwise ->
                      plainMonadoHudCell MonadoHudToneWarn "no GC"
        let missingOpenXRLatestCell latestActual =
              case latestActual of
                Just _ ->
                  plainMonadoHudCell MonadoHudToneActual (formatMaybeMsOne latestActual)
                Nothing ->
                  plainMonadoHudCell MonadoHudToneWarn "no OpenXR"
        let rows =
              [ eventRow
                  "compositor probably missed frame warning"
                  (debugMonadoHudSnapshotDisplayMissCount snapshot)
                  (debugMonadoHudSnapshotLatestDisplayMissMs snapshot)
                  (debugMonadoHudSnapshotWorstDisplayMissWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstDisplayMissSessionMs snapshot)
              , eventRow
                  "stale late frame dropped"
                  (debugMonadoHudSnapshotOldFrameDropCount snapshot)
                  Nothing
                  Nothing
                  Nothing
              , eventRow
                  "frame missed"
                  (debugMonadoHudSnapshotPacingMissCount snapshot)
                  Nothing
                  Nothing
                  Nothing
              , actualOnlyTimingRowWithLatestCell
                  "godot pre-openxr work"
                  (debugMonadoHudSnapshotOpenXRFrameTimingSampleCount snapshot)
                  (missingOpenXRLatestCell (openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs <$> debugMonadoHudSnapshotLatestOpenXRFrameTimingSample snapshot))
                  (debugMonadoHudSnapshotAvgOpenXRGodotFrameStartToXrWaitFrameMs snapshot)
                  (debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstOpenXRGodotFrameStartToXrWaitFrameSessionMs snapshot)
                  False
              , actualOnlyTimingRowWithLatestCell
                  "  GC time"
                  (debugMonadoHudSnapshotGhcGCFrameTimingSampleCount snapshot)
                  (missingGCLatestCell (ghcGCFrameTimingSampleGcMs <$> debugMonadoHudSnapshotLatestGhcGCFrameTimingSample snapshot))
                  (debugMonadoHudSnapshotAvgGhcGCTimeMs snapshot)
                  (debugMonadoHudSnapshotWorstGhcGCTimeWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstGhcGCTimeSessionMs snapshot)
                  False
              , actualOnlyTimingRowWithLatestCell
                  "  non-GC time"
                  (debugMonadoHudSnapshotGhcGCFrameTimingSampleCount snapshot)
                  (missingGCLatestCell (ghcGCFrameTimingSampleNonGCGodotFrameStartToXrWaitFrameMs <$> debugMonadoHudSnapshotLatestGhcGCFrameTimingSample snapshot))
                  (debugMonadoHudSnapshotAvgNonGCGodotFrameStartToXrWaitFrameMs snapshot)
                  (debugMonadoHudSnapshotWorstNonGCGodotFrameStartToXrWaitFrameWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstNonGCGodotFrameStartToXrWaitFrameSessionMs snapshot)
                  False
              , actualOnlyTimingRowWithLatestCell
                  "xrWaitFrame sleep"
                  (debugMonadoHudSnapshotOpenXRFrameTimingSampleCount snapshot)
                  (missingOpenXRLatestCell (openXRFrameTimingSampleXrWaitFrameSleepMs <$> debugMonadoHudSnapshotLatestOpenXRFrameTimingSample snapshot))
                  (debugMonadoHudSnapshotAvgOpenXRXrWaitFrameSleepMs snapshot)
                  (debugMonadoHudSnapshotWorstOpenXRXrWaitFrameSleepWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstOpenXRXrWaitFrameSleepSessionMs snapshot)
                  False
              , timingRow
                  "monado app (openxr rendering) time"
                  latestAppEstimate
                  latestAppActual
                  (debugMonadoHudSnapshotAvgAppEstimateMs snapshot)
                  (debugMonadoHudSnapshotAvgAppObservedMs snapshot)
                  (debugMonadoHudSnapshotWorstAppEstimateWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppObservedWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppEstimateSessionMs snapshot)
                  (debugMonadoHudSnapshotWorstAppObservedSessionMs snapshot)
                  False
              , timingRow
                  "  cpu time"
                  (monadoAppCpuEstimateMs <$> appTiming)
                  (monadoAppCpuObservedMs <$> appTiming)
                  (debugMonadoHudSnapshotAvgAppCpuEstimateMs snapshot)
                  (debugMonadoHudSnapshotAvgAppCpuObservedMs snapshot)
                  (debugMonadoHudSnapshotWorstAppCpuEstimateWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppCpuObservedWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppCpuEstimateSessionMs snapshot)
                  (debugMonadoHudSnapshotWorstAppCpuObservedSessionMs snapshot)
                  False
              , timingRow
                  "  draw time"
                  (monadoAppDrawEstimateMs <$> appTiming)
                  (monadoAppDrawObservedMs <$> appTiming)
                  (debugMonadoHudSnapshotAvgAppDrawEstimateMs snapshot)
                  (debugMonadoHudSnapshotAvgAppDrawObservedMs snapshot)
                  (debugMonadoHudSnapshotWorstAppDrawEstimateWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppDrawObservedWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppDrawEstimateSessionMs snapshot)
                  (debugMonadoHudSnapshotWorstAppDrawObservedSessionMs snapshot)
                  False
              , timingRow
                  "  gpu time"
                  (monadoAppGpuEstimateMs <$> appTiming)
                  (monadoAppGpuObservedMs <$> appTiming)
                  (debugMonadoHudSnapshotAvgAppGpuEstimateMs snapshot)
                  (debugMonadoHudSnapshotAvgAppGpuObservedMs snapshot)
                  (debugMonadoHudSnapshotWorstAppGpuEstimateWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppGpuObservedWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstAppGpuEstimateSessionMs snapshot)
                  (debugMonadoHudSnapshotWorstAppGpuObservedSessionMs snapshot)
                  False
              , timingRow
                  "monado compositor time"
                  unknown
                  latestCompositorActual
                  unknown
                  avgCompositorActual
                  unknown
                  worstWindowCompositorActual
                  unknown
                  worstSessionCompositorActual
                  False
              , timingRow
                  "  cpu time"
                  unknown
                  latestCompositorCpuActual
                  unknown
                  (debugMonadoHudSnapshotAvgCompositorMs snapshot)
                  unknown
                  (debugMonadoHudSnapshotWorstCompositorWindowMs snapshot)
                  unknown
                  (debugMonadoHudSnapshotWorstCompositorSessionMs snapshot)
                  False
              , timingRow
                  "  gpu time"
                  unknown
                  latestCompositorGpuActual
                  unknown
                  (debugMonadoHudSnapshotAvgCompositorSubmitToInferredGpuDoneMs snapshot)
                  unknown
                  (debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneWindowMs snapshot)
                  unknown
                  (debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneSessionMs snapshot)
                  False
              , timingRow
                  "monado margins"
                  latestEstimatedMargin
                  (compTiming >>= monadoCompositorPresentMarginMs)
                  (debugMonadoHudSnapshotAvgCompositorInferredGpuDoneToDesiredMs snapshot)
                  (debugMonadoHudSnapshotAvgPresentMarginMs snapshot)
                  (debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstPresentMarginWindowMs snapshot)
                  (debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredSessionMs snapshot)
                  (debugMonadoHudSnapshotWorstPresentMarginSessionMs snapshot)
                  (maybe False (<= 0.5) latestMarginForWarn)
              , timingRow
                  "total (without sleep/margins)"
                  unknown
                  latestKnownWithoutSleepActual
                  unknown
                  avgKnownWithoutSleepActual
                  unknown
                  worstWindowKnownWithoutSleepActual
                  unknown
                  worstSessionKnownWithoutSleepActual
                  False
              , timingRow
                  "total"
                  unknown
                  latestTotalActual
                  unknown
                  avgTotalActual
                  unknown
                  unknown
                  unknown
                  unknown
                  False
              ]
        let colorForTone tone =
              case tone of
                MonadoHudToneNeutral -> neutralColor
                MonadoHudToneEstimate -> estimateColor
                MonadoHudToneActual -> actualColor
                MonadoHudToneDeltaAhead -> aheadColor
                MonadoHudToneDeltaMiss -> hotColor
                MonadoHudToneHot -> hotColor
                MonadoHudToneWarn -> warnColor
        forM_ (take rowsAvailable $ zip [0 :: Int ..] rows) $ \(rowIndex, row) -> do
          let sectionGap = if rowIndex >= 3 then lineStep * 2 else 0
          let baseline = firstBaseline + lineStep * fromIntegral rowIndex + sectionGap
          let color =
                if monadoHudRowHot row
                  then hotColor
                  else
                    if monadoHudRowWarn row
                      then warnColor
                      else neutralColor
          drawBoldText (monadoHudRowMetric row) color left baseline metricWidth
          drawRightAlignedText (monadoHudRowCount row) color countRight baseline countWidth
          drawRightAlignedText (monadoHudRowRate row) color rateRight baseline rateWidth
          drawRightAlignedSegments colorForTone (monadoHudRowLatest row) latestRight baseline latestWidth
          drawRightAlignedSegments colorForTone (monadoHudRowAverage row) avgRight baseline avgWidth
          drawRightAlignedSegments colorForTone (monadoHudRowWorstWindow row) worstWindowRight baseline worstWindowWidth
          drawRightAlignedSegments colorForTone (monadoHudRowWorstSession row) worstSessionRight baseline worstSessionWidth
          drawHorizontalLine lineColor (baseline + 5)
        case debugMonadoHudSnapshotErrors snapshot of
          err : _ -> do
            let visibleRows = min rowsAvailable (List.length rows)
            let sectionGap = if visibleRows >= 3 then lineStep * 2 else 0
            let baseline = firstBaseline + lineStep * fromIntegral visibleRows + sectionGap
            drawText (fitTextForWidth availableWidth err) hotColor left baseline availableWidth
          [] -> return ()

    rowsAvailable =
      max 0 (floor ((availableHeight - 4 * debugMonadoHudLineStepPixels) / debugMonadoHudLineStepPixels) :: Int)

    drawEventStrip stripTop stripHeight = do
      displayMissColor <- (toLowLevel $ (rgb 1.0 0.22 0.16) `withOpacity` 0.98) :: IO GodotColor
      oldDropColor <- (toLowLevel $ (rgb 1.0 0.76 0.18) `withOpacity` 0.98) :: IO GodotColor
      quietColor <- (toLowLevel $ (rgb 0.22 0.80 0.34) `withOpacity` 0.66) :: IO GodotColor
      emptyColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.10) :: IO GodotColor
      let buckets = debugMonadoHudSnapshotFrameSkipEventBuckets snapshot
      let bucketCount = max 1 (List.length buckets)
      let gap = 1
      let bucketWidth = max 1 ((availableWidth - fromIntegral (bucketCount - 1) * gap) / fromIntegral bucketCount)
      forM_ buckets $ \bucket -> do
        let index = monadoFrameSkipEventBucketIndex bucket
        let x = left + fromIntegral index * (bucketWidth + gap)
        let eventCount = monadoFrameSkipEventBucketEventCount bucket
        let heightFraction
              | eventCount <= 0 = 0.28
              | monadoFrameSkipEventBucketDisplayMiss bucket = 1.0
              | monadoFrameSkipEventBucketOldDrop bucket = 0.72
              | otherwise = 0.55
        let h = max 2 (stripHeight * heightFraction)
        let y = stripTop + stripHeight - h
        rect <- toLowLevel (V2 (V2 x y) (V2 bucketWidth h)) :: IO GodotRect2
        G.draw_rect cb rect (bucketColor emptyColor quietColor oldDropColor displayMissColor bucket) True 1.0 False

    bucketColor emptyColor quietColor oldDropColor displayMissColor bucket
      | monadoFrameSkipEventBucketEventCount bucket <= 0 = emptyColor
      | monadoFrameSkipEventBucketDisplayMiss bucket = displayMissColor
      | monadoFrameSkipEventBucketOldDrop bucket = oldDropColor
      | otherwise = quietColor

    drawHorizontalLine color y = do
      start <- toLowLevel (V2 left y) :: IO GodotVector2
      end <- toLowLevel (V2 (left + availableWidth) y) :: IO GodotVector2
      G.draw_line cb start end color 1 False

    drawText text color x baseline maxWidth =
      bracket
        (toLowLevel (pack text) :: IO GodotString)
        Api.godot_string_destroy
        (\textStr -> do
          renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
          G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr color (round maxWidth))

    drawBoldText text color x baseline maxWidth = do
      drawText text color x baseline maxWidth
      drawText text color (x + 1) baseline maxWidth

    drawRightAlignedText text color right baseline maxWidth =
      bracket
        (toLowLevel (pack text) :: IO GodotString)
        Api.godot_string_destroy
        (\textStr -> do
          V2 textWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) textStr >>= fromLowLevel :: IO (V2 Float)
          let x = max (right - maxWidth) (right - textWidth)
          renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
          G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr color (round maxWidth))

    drawRightAlignedBoldText text color right baseline maxWidth = do
      drawRightAlignedText text color right baseline maxWidth
      drawRightAlignedText text color (right + 1) baseline maxWidth

    drawRightAlignedSegments colorForTone segments right baseline maxWidth = do
      widths <- mapM segmentWidth segments
      let totalWidth = sum widths
      let x = max (right - maxWidth) (right - totalWidth)
      drawSegmentRun x (zip segments widths)
      where
        segmentWidth segment =
          bracket
            (toLowLevel (pack $ monadoHudCellSegmentText segment) :: IO GodotString)
            Api.godot_string_destroy
            (\textStr -> do
              V2 textWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) textStr >>= fromLowLevel :: IO (V2 Float)
              return textWidth)

        drawSegmentRun _ [] = return ()
        drawSegmentRun x ((segment, width) : rest) = do
          bracket
            (toLowLevel (pack $ monadoHudCellSegmentText segment) :: IO GodotString)
            Api.godot_string_destroy
            (\textStr -> do
              renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
              G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr (colorForTone $ monadoHudCellSegmentTone segment) (round maxWidth))
          drawSegmentRun (x + width) rest

formatMonadoHudHeader :: DebugMonadoHudSnapshot -> String
formatMonadoHudHeader snapshot =
  "MONADO"
    ++ " window="
    ++ formatSecondsValue (debugMonadoHudSnapshotWindowSeconds snapshot)
    ++ "s comp_warn="
    ++ show (debugMonadoHudSnapshotDisplayMissCount snapshot)
    ++ " stale_drop="
    ++ show (debugMonadoHudSnapshotOldFrameDropCount snapshot)
    ++ " app_obs="
    ++ formatMaybeMs (monadoAppObservedTotalMs <$> debugMonadoHudSnapshotLatestAppTiming snapshot)
    ++ " comp="
    ++ formatMaybeMs (debugMonadoHudSnapshotLatestCompositorTiming snapshot >>= monadoCompositorWakeToSubmitMs)

formatMaybeMs :: Maybe Double -> String
formatMaybeMs Nothing = "n/a"
formatMaybeMs (Just value) = formatMsValue value ++ "ms"

formatMaybeBool :: Maybe Bool -> String
formatMaybeBool Nothing = "n/a"
formatMaybeBool (Just True) = "true"
formatMaybeBool (Just False) = "false"

formatMaybeMsOne :: Maybe Double -> String
formatMaybeMsOne Nothing = "?"
formatMaybeMsOne (Just value) = formatMsValueOne value ++ "ms"

formatMsValue :: Double -> String
formatMsValue value = showFFloat (Just 2) value ""

formatMsValueOne :: Double -> String
formatMsValueOne value = showFFloat (Just 1) value ""

formatSecondsValue :: Double -> String
formatSecondsValue value =
  if abs (value - fromInteger (round value)) < 0.005
    then show (round value :: Int)
    else showFFloat (Just 2) value ""

plainMonadoHudCell :: MonadoHudCellTone -> String -> [MonadoHudCellSegment]
plainMonadoHudCell tone text =
  [MonadoHudCellSegment tone text]

monadoHudEstimateActualDeltaCell :: Maybe Double -> Maybe Double -> [MonadoHudCellSegment]
monadoHudEstimateActualDeltaCell estimated actual =
  [ MonadoHudCellSegment MonadoHudToneEstimate (formatMaybeBare estimated)
  , MonadoHudCellSegment MonadoHudToneNeutral " - "
  , MonadoHudCellSegment MonadoHudToneActual (formatMaybeBare actual)
  , MonadoHudCellSegment MonadoHudToneNeutral " = "
  , MonadoHudCellSegment deltaTone (formatDelta estimated actual)
  ]
  where
    formatMaybeBare Nothing = "?"
    formatMaybeBare (Just value) = formatMsValueOne value

    formatDelta maybeEstimated maybeActual =
      case (-) <$> maybeEstimated <*> maybeActual of
        Nothing -> "?"
        Just value -> signedValue value ++ "ms"

    signedValue value =
      (if value >= 0 then "+" else "") ++ formatMsValueOne value

    deltaTone =
      case (-) <$> estimated <*> actual of
        Just value
          | value < 0 -> MonadoHudToneDeltaMiss
          | otherwise -> MonadoHudToneDeltaAhead
        Nothing -> MonadoHudToneNeutral

padRight :: Int -> String -> String
padRight width text =
  text ++ List.replicate (max 0 (width - List.length text)) ' '

padLeft :: Int -> String -> String
padLeft width text =
  List.replicate (max 0 (width - List.length text)) ' ' ++ text

fitTextForWidth :: Float -> String -> String
fitTextForWidth maxWidth text =
  let maxChars = max 4 (floor (maxWidth / 8) :: Int)
   in if List.length text <= maxChars
        then text
        else List.take (max 1 $ maxChars - 3) text ++ "..."
