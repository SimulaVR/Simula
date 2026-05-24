module Plugin.Debug.MonadoHudTypes where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Time.Clock
import System.Environment
import System.IO.Unsafe
import Text.Read (readMaybe)

import Plugin.Debug.HudTypes

debugMonadoHudEnabled :: IO Bool
debugMonadoHudEnabled =
  debugHudModeActive DebugHudMonado

debugMonadoHudHeight :: Int
debugMonadoHudHeight = 470

debugMonadoHudLineHeightPixels :: Int
debugMonadoHudLineHeightPixels = 18

debugMonadoHudFontAscentPixels :: Float
debugMonadoHudFontAscentPixels = 14

debugMonadoHudLineStepPixels :: Float
debugMonadoHudLineStepPixels =
  fromIntegral debugMonadoHudLineHeightPixels

debugMonadoHudWindowSeconds :: Double
debugMonadoHudWindowSeconds = unsafePerformIO $ readDoubleEnv "SIMULA_DEBUG_MONADO_HUD_WINDOW_S" 10.0
{-# NOINLINE debugMonadoHudWindowSeconds #-}

debugMonadoHudWindowInSeconds :: NominalDiffTime
debugMonadoHudWindowInSeconds = realToFrac debugMonadoHudWindowSeconds

debugMonadoHudPresentToDisplayOffsetMs :: Double
debugMonadoHudPresentToDisplayOffsetMs = unsafePerformIO $ readDoubleEnv "SIMULA_MONADO_HUD_PRESENT_TO_DISPLAY_MS" 4.0
{-# NOINLINE debugMonadoHudPresentToDisplayOffsetMs #-}

debugMonadoHudSnapshotIntervalInSeconds :: NominalDiffTime
debugMonadoHudSnapshotIntervalInSeconds = 1.0

debugMonadoHudOutputIntervalInSeconds :: NominalDiffTime
debugMonadoHudOutputIntervalInSeconds = 1.0

debugMonadoHudFrameSkipEventBucketCount :: Int
debugMonadoHudFrameSkipEventBucketCount = 60

debugMonadoHudMaxEvents :: Int
debugMonadoHudMaxEvents = 1024

debugMonadoHudMaxTimingSamples :: Int
debugMonadoHudMaxTimingSamples = 1024

data MonadoHudFrameSkipEventKind
  = MonadoHudDisplayMiss
  | MonadoHudOldFrameDrop
  | MonadoHudPacingMiss
  | MonadoHudPredictionSkip
  deriving (Eq, Ord, Show)

data MonadoHudFrameSkipEvent = MonadoHudFrameSkipEvent
  { -- Simula receipt time for the parsed Monado warning line.
    monadoHudFrameSkipEventTime :: UTCTime
  -- Frame-skip category inferred by string matching the Monado log line.
  , monadoHudFrameSkipEventKind :: MonadoHudFrameSkipEventKind
  -- Parsed from "missed frame by N.ms" or "missed by N.ms" when present.
  , monadoHudFrameSkipEventMs   :: Maybe Double
  -- Original Monado log line, truncated when stored.
  , monadoHudFrameSkipEventLine :: String
  }
  deriving (Show)

data MonadoAppTiming = MonadoAppTiming
  { -- Parsed from "Delivered frame N.ms ...".
    monadoAppDeliveredDeltaMs :: Maybe Double
  -- True when the delivered-frame line contains "late".
  , monadoAppDeliveredLate    :: Bool
  -- Parsed from the app pacing "period:" line.
  , monadoAppPeriodMs         :: Maybe Double
  -- Parsed from "cpu  o: estimate, n: observed"; this is Monado's old/estimated CPU value.
  , monadoAppCpuEstimateMs    :: Double
  -- Parsed from "cpu  o: estimate, n: observed"; this is Monado's new/observed CPU value.
  , monadoAppCpuObservedMs    :: Double
  -- Parsed from "draw o: estimate, n: observed"; this is Monado's old/estimated draw value.
  , monadoAppDrawEstimateMs   :: Double
  -- Parsed from "draw o: estimate, n: observed"; this is Monado's new/observed draw value.
  , monadoAppDrawObservedMs   :: Double
  -- Parsed from "gpu  o: estimate, n: observed"; this is Monado's old/estimated GPU value.
  , monadoAppGpuEstimateMs    :: Double
  -- Parsed from "gpu  o: estimate, n: observed"; this is Monado's new/observed GPU value.
  , monadoAppGpuObservedMs    :: Double
  }
  deriving (Show)

data MonadoCompositorTiming = MonadoCompositorTiming
  { -- (when_submitted_ns - when_woke_ns) / 1e6; HUD "compositor cpu time" actual.
    monadoCompositorWakeToSubmitMs                 :: Maybe Double
  -- ((actual_present_time_ns - present_margin_ns) - when_submitted_ns) / 1e6; inferred HUD "compositor gpu time" actual.
  , monadoCompositorSubmitToInferredGpuDoneMs      :: Maybe Double
  -- (desired_present_time_ns - (actual_present_time_ns - present_margin_ns)) / 1e6; slack from inferred GPU done to desired present.
  , monadoCompositorInferredGpuDoneToDesiredMs     :: Maybe Double
  -- (actual_present_time_ns - when_submitted_ns) / 1e6; post-submit time until actual present feedback.
  , monadoCompositorSubmitToActualPresentMs        :: Maybe Double
  -- (actual_present_time_ns - when_woke_ns) / 1e6; full compositor wake-to-present interval.
  , monadoCompositorWakeToActualPresentMs          :: Maybe Double
  -- (actual_present_time_ns - desired_present_time_ns) / 1e6; positive means actual present was late.
  , monadoCompositorDesiredToActualPresentMs       :: Maybe Double
  -- (actual_present_time_ns - earliest_present_time_ns) / 1e6; actual present relative to earliest present.
  , monadoCompositorEarliestToActualPresentMs      :: Maybe Double
  -- (when_infoed_ns - actual_present_time_ns) / 1e6; delay before Monado got/processed present feedback.
  , monadoCompositorInfoAfterActualPresentMs       :: Maybe Double
  -- Parsed from "present_margin_ms"; HUD "monado margins" actual, where smaller is worse.
  , monadoCompositorPresentMarginMs                :: Maybe Double
  -- Parsed from "since_last_frame_ms"; compositor frame-to-frame cadence.
  , monadoCompositorSinceLastFrameMs               :: Maybe Double
  }
  deriving (Show)

data MonadoTimed a = MonadoTimed
  { -- Simula receipt time for the completed parsed sample.
    monadoTimedAt    :: UTCTime
  -- Parsed app/compositor timing value associated with monadoTimedAt.
  , monadoTimedValue :: a
  }
  deriving (Show)

data PendingMonadoAppTiming = PendingMonadoAppTiming
  { -- Parsed from "Delivered frame N.ms ..."; copied to monadoAppDeliveredDeltaMs on completion.
    pendingAppDeliveredDeltaMs :: Maybe Double
  -- True when the delivered-frame line contains "late"; copied to monadoAppDeliveredLate.
  , pendingAppDeliveredLate    :: Bool
  -- Parsed from "period:"; copied to monadoAppPeriodMs.
  , pendingAppPeriodMs         :: Maybe Double
  -- Parsed from "cpu  o: estimate, n: observed"; completed into CPU estimate/observed fields.
  , pendingAppCpu              :: Maybe (Double, Double)
  -- Parsed from "draw o: estimate, n: observed"; completed into draw estimate/observed fields.
  , pendingAppDraw             :: Maybe (Double, Double)
  -- Parsed from "gpu  o: estimate, n: observed"; completed into GPU estimate/observed fields.
  , pendingAppGpu              :: Maybe (Double, Double)
  }
  deriving (Show)

data PendingMonadoCompositorTiming = PendingMonadoCompositorTiming
  { -- Parsed from "when_woke_ns:"; start point for wake-to-submit and wake-to-present.
    pendingCompositorWhenWokeNs       :: Maybe Integer
  -- Parsed from "when_submitted_ns:"; end of compositor CPU work and start point for inferred GPU tail.
  , pendingCompositorWhenSubmittedNs  :: Maybe Integer
  -- Parsed from "when_infoed_ns:"; used for info-after-actual-present.
  , pendingCompositorWhenInfoedNs     :: Maybe Integer
  -- Parsed from "desired_present_time_ns:"; used for desired-to-actual and inferred-GPU-done-to-desired.
  , pendingCompositorDesiredPresentNs :: Maybe Integer
  -- Parsed from "actual_present_time_ns:"; actual present/scanout timestamp used by several derived intervals.
  , pendingCompositorActualPresentNs  :: Maybe Integer
  -- Parsed from "earliest_present_time_ns:"; used for earliest-to-actual-present.
  , pendingCompositorEarliestPresentNs :: Maybe Integer
  -- Parsed from "present_margin_ns:"; actual_present_time_ns - present_margin_ns gives inferred GPU-done ns.
  , pendingCompositorPresentMarginNs  :: Maybe Integer
  -- Parsed from "present_margin_ms:"; completed into monadoCompositorPresentMarginMs.
  , pendingCompositorPresentMarginMs  :: Maybe Double
  -- Parsed from "since_last_frame_ms:"; completed into monadoCompositorSinceLastFrameMs.
  , pendingCompositorSinceLastFrameMs :: Maybe Double
  }
  deriving (Show)

data MonadoHudState = MonadoHudState
  { monadoHudStateLogPath                 :: Maybe FilePath
  , monadoHudStateLogOffset               :: Integer
  , monadoHudStatePartialLine             :: String
  , monadoHudStateFrameSkipEvents         :: Seq MonadoHudFrameSkipEvent
  , monadoHudStateAppTimingSamples        :: Seq (MonadoTimed MonadoAppTiming)
  , monadoHudStateCompositorTimingSamples :: Seq (MonadoTimed MonadoCompositorTiming)
  , monadoHudStateWorstDisplayMissSessionMs :: Maybe Double
  , monadoHudStateWorstAppObservedSessionMs :: Maybe Double
  , monadoHudStateWorstAppEstimateSessionMs :: Maybe Double
  , monadoHudStateWorstAppCpuEstimateSessionMs :: Maybe Double
  , monadoHudStateWorstAppCpuObservedSessionMs :: Maybe Double
  , monadoHudStateWorstAppDrawEstimateSessionMs :: Maybe Double
  , monadoHudStateWorstAppDrawObservedSessionMs :: Maybe Double
  , monadoHudStateWorstAppGpuEstimateSessionMs :: Maybe Double
  , monadoHudStateWorstAppGpuObservedSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorSessionMs  :: Maybe Double
  , monadoHudStateWorstCompositorSubmitToInferredGpuDoneSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorInferredGpuDoneToDesiredSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorSubmitToActualPresentSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorWakeToActualPresentSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorDesiredToActualPresentSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorEarliestToActualPresentSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorInfoAfterActualPresentSessionMs :: Maybe Double
  , monadoHudStateWorstPresentMarginSessionMs :: Maybe Double
  , monadoHudStateWorstCompositorSinceLastFrameSessionMs :: Maybe Double
  , monadoHudStatePendingAppTiming        :: Maybe PendingMonadoAppTiming
  , monadoHudStatePendingCompositorTiming :: Maybe PendingMonadoCompositorTiming
  , monadoHudStateErrors                  :: [String]
  }
  deriving (Show)

data MonadoFrameSkipEventBucket = MonadoFrameSkipEventBucket
  { monadoFrameSkipEventBucketIndex       :: Int
  , monadoFrameSkipEventBucketEventCount  :: Int
  , monadoFrameSkipEventBucketDisplayMiss :: Bool
  , monadoFrameSkipEventBucketOldDrop     :: Bool
  , monadoFrameSkipEventBucketWorstMs     :: Maybe Double
  }
  deriving (Show)

data DebugMonadoHudSnapshot = DebugMonadoHudSnapshot
  { debugMonadoHudSnapshotUpdatedAt              :: UTCTime
  , debugMonadoHudSnapshotLogPath                :: FilePath
  , debugMonadoHudSnapshotWindowSeconds          :: Double
  , debugMonadoHudSnapshotDisplayMissCount       :: Int
  , debugMonadoHudSnapshotOldFrameDropCount      :: Int
  , debugMonadoHudSnapshotPacingMissCount        :: Int
  , debugMonadoHudSnapshotPredictionSkipCount    :: Int
  , debugMonadoHudSnapshotLatestDisplayMissMs    :: Maybe Double
  , debugMonadoHudSnapshotWorstDisplayMissWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstDisplayMissSessionMs :: Maybe Double
  , debugMonadoHudSnapshotLatestAppTiming        :: Maybe MonadoAppTiming
  , debugMonadoHudSnapshotWorstAppObservedWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppObservedSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppObservedMs       :: Maybe Double
  , debugMonadoHudSnapshotWorstAppEstimateWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppEstimateSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppEstimateMs       :: Maybe Double
  , debugMonadoHudSnapshotWorstAppCpuEstimateWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppCpuEstimateSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppCpuEstimateMs    :: Maybe Double
  , debugMonadoHudSnapshotWorstAppCpuObservedWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppCpuObservedSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppCpuObservedMs    :: Maybe Double
  , debugMonadoHudSnapshotWorstAppDrawEstimateWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppDrawEstimateSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppDrawEstimateMs   :: Maybe Double
  , debugMonadoHudSnapshotWorstAppDrawObservedWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppDrawObservedSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppDrawObservedMs   :: Maybe Double
  , debugMonadoHudSnapshotWorstAppGpuEstimateWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppGpuEstimateSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppGpuEstimateMs    :: Maybe Double
  , debugMonadoHudSnapshotWorstAppGpuObservedWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstAppGpuObservedSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgAppGpuObservedMs    :: Maybe Double
  , debugMonadoHudSnapshotLatestCompositorTiming :: Maybe MonadoCompositorTiming
  , debugMonadoHudSnapshotWorstCompositorWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorMs        :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorSubmitToInferredGpuDoneMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorInferredGpuDoneToDesiredMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorSubmitToActualPresentMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorWakeToActualPresentWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorWakeToActualPresentSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorWakeToActualPresentMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorDesiredToActualPresentMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorEarliestToActualPresentMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorInfoAfterActualPresentMs :: Maybe Double
  , debugMonadoHudSnapshotWorstPresentMarginWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstPresentMarginSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgPresentMarginMs     :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSinceLastFrameWindowMs :: Maybe Double
  , debugMonadoHudSnapshotWorstCompositorSinceLastFrameSessionMs :: Maybe Double
  , debugMonadoHudSnapshotAvgCompositorSinceLastFrameMs :: Maybe Double
  , debugMonadoHudSnapshotFrameSkipEventBuckets :: [MonadoFrameSkipEventBucket]
  , debugMonadoHudSnapshotErrors                 :: [String]
  }
  deriving (Show)

emptyMonadoHudState :: MonadoHudState
emptyMonadoHudState =
  MonadoHudState
    { monadoHudStateLogPath = Nothing
    , monadoHudStateLogOffset = 0
    , monadoHudStatePartialLine = ""
    , monadoHudStateFrameSkipEvents = Seq.empty
    , monadoHudStateAppTimingSamples = Seq.empty
    , monadoHudStateCompositorTimingSamples = Seq.empty
    , monadoHudStateWorstDisplayMissSessionMs = Nothing
    , monadoHudStateWorstAppObservedSessionMs = Nothing
    , monadoHudStateWorstAppEstimateSessionMs = Nothing
    , monadoHudStateWorstAppCpuEstimateSessionMs = Nothing
    , monadoHudStateWorstAppCpuObservedSessionMs = Nothing
    , monadoHudStateWorstAppDrawEstimateSessionMs = Nothing
    , monadoHudStateWorstAppDrawObservedSessionMs = Nothing
    , monadoHudStateWorstAppGpuEstimateSessionMs = Nothing
    , monadoHudStateWorstAppGpuObservedSessionMs = Nothing
    , monadoHudStateWorstCompositorSessionMs = Nothing
    , monadoHudStateWorstCompositorSubmitToInferredGpuDoneSessionMs = Nothing
    , monadoHudStateWorstCompositorInferredGpuDoneToDesiredSessionMs = Nothing
    , monadoHudStateWorstCompositorSubmitToActualPresentSessionMs = Nothing
    , monadoHudStateWorstCompositorWakeToActualPresentSessionMs = Nothing
    , monadoHudStateWorstCompositorDesiredToActualPresentSessionMs = Nothing
    , monadoHudStateWorstCompositorEarliestToActualPresentSessionMs = Nothing
    , monadoHudStateWorstCompositorInfoAfterActualPresentSessionMs = Nothing
    , monadoHudStateWorstPresentMarginSessionMs = Nothing
    , monadoHudStateWorstCompositorSinceLastFrameSessionMs = Nothing
    , monadoHudStatePendingAppTiming = Nothing
    , monadoHudStatePendingCompositorTiming = Nothing
    , monadoHudStateErrors = []
    }

emptyPendingMonadoCompositorTiming :: PendingMonadoCompositorTiming
emptyPendingMonadoCompositorTiming =
  PendingMonadoCompositorTiming
    { pendingCompositorWhenWokeNs = Nothing
    , pendingCompositorWhenSubmittedNs = Nothing
    , pendingCompositorWhenInfoedNs = Nothing
    , pendingCompositorDesiredPresentNs = Nothing
    , pendingCompositorActualPresentNs = Nothing
    , pendingCompositorEarliestPresentNs = Nothing
    , pendingCompositorPresentMarginNs = Nothing
    , pendingCompositorPresentMarginMs = Nothing
    , pendingCompositorSinceLastFrameMs = Nothing
    }

monadoAppObservedTotalMs :: MonadoAppTiming -> Double
monadoAppObservedTotalMs timing =
  -- HUD "monado app (openxr rendering) time" actual.
  monadoAppCpuObservedMs timing + monadoAppDrawObservedMs timing + monadoAppGpuObservedMs timing

monadoAppEstimatedTotalMs :: MonadoAppTiming -> Double
monadoAppEstimatedTotalMs timing =
  -- HUD "monado app (openxr rendering) time" estimate.
  monadoAppCpuEstimateMs timing + monadoAppDrawEstimateMs timing + monadoAppGpuEstimateMs timing

monadoEstimatedAppCompositorSubmitMs :: MonadoAppTiming -> MonadoCompositorTiming -> Maybe Double
monadoEstimatedAppCompositorSubmitMs appTiming compositorTiming =
  -- App observed total plus compositor wake-to-submit.
  (monadoAppObservedTotalMs appTiming +) <$> monadoCompositorWakeToSubmitMs compositorTiming

monadoEstimatedAppCompositorActualPresentMs :: MonadoAppTiming -> MonadoCompositorTiming -> Maybe Double
monadoEstimatedAppCompositorActualPresentMs appTiming compositorTiming =
  -- App observed total plus compositor wake-to-actual-present.
  (monadoAppObservedTotalMs appTiming +) <$> monadoCompositorWakeToActualPresentMs compositorTiming

monadoEstimatedAppCompositorVisibleMs :: MonadoAppTiming -> MonadoCompositorTiming -> Maybe Double
monadoEstimatedAppCompositorVisibleMs appTiming compositorTiming =
  -- App + compositor actual-present estimate plus the configured photons/display visibility offset.
  (debugMonadoHudPresentToDisplayOffsetMs +) <$> monadoEstimatedAppCompositorActualPresentMs appTiming compositorTiming

formatMonadoFrameSkipEventKind :: MonadoHudFrameSkipEventKind -> String
formatMonadoFrameSkipEventKind MonadoHudDisplayMiss = "compositor_probably_missed_frame_warning"
formatMonadoFrameSkipEventKind MonadoHudOldFrameDrop = "stale_late_frame_dropped"
formatMonadoFrameSkipEventKind MonadoHudPacingMiss = "frame_missed"
formatMonadoFrameSkipEventKind MonadoHudPredictionSkip = "prediction_skip"

ratePerMinute :: Int -> Double
ratePerMinute count =
  if debugMonadoHudWindowSeconds <= 0
    then 0
    else fromIntegral count * 60.0 / debugMonadoHudWindowSeconds

readDoubleEnv :: String -> Double -> IO Double
readDoubleEnv name fallback = do
  maybeValue <- lookupEnv name
  return $
    case maybeValue >>= readMaybe of
      Just value -> value
      Nothing -> fallback
