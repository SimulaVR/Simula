module Plugin.Debug.MonadoHudParse where

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe
import Text.Read (readMaybe)

import Plugin.Debug.MonadoHudTypes

-- Accepted Monado HUD log line examples:
--
-- Frame skip events:
--   WARN [comp_renderer_draw] Compositor probably missed frame by 16.75ms
--   WARN [wait_for_scheduled_free] 9066425.898ms: Dropping old missed frame in favour for completed new frame
--   TRACE [u_pacing_app] Frame 18422 missed by 2.41ms, predicted display time was too early
--   TRACE [u_pacing_app] Skipped! predicted frame was already too late for pacing
--
-- App timing block:
--   TRACE [u_pacing_app] Delivered frame 0.34ms late
--   TRACE [u_pacing_app]     period: 11.111ms
--   TRACE [u_pacing_app]     cpu  o: 2.15ms n: 2.60ms
--   TRACE [u_pacing_app]     draw o: 3.40ms n: 4.10ms
--   TRACE [u_pacing_app]     gpu  o: 5.20ms n: 6.05ms
--
-- Compositor timing block:
--   TRACE [pc_info] Got
--   TRACE [u_pacing_compositor]     since_last_frame_ms: 11.11
--   TRACE [u_pacing_compositor]     when_woke_ns: 9066425898123456
--   TRACE [u_pacing_compositor]     when_submitted_ns: 9066425901567890
--   TRACE [u_pacing_compositor]     desired_present_time_ns: 9066425910000000
--   TRACE [u_pacing_compositor]     gpu_end_ns: 9066425905500000
--   TRACE [u_pacing_compositor]     actual_present_time_ns: 9066425916800000
--   TRACE [u_pacing_compositor]     earliest_present_time_ns: 9066425909200000
--   TRACE [u_pacing_compositor]     when_infoed_ns: 9066425919100000
--   TRACE [u_pacing_compositor]     present_margin_ns: 4200000
--   TRACE [u_pacing_compositor]     present_margin_ms: 4.20

-- Examples:
--   WARN [comp_renderer_draw] Compositor probably missed frame by 16.75ms
--   WARN [wait_for_scheduled_free] 9066425.898ms: Dropping old missed frame in favour for completed new frame
--   TRACE [u_pacing_app] Frame 18422 missed by 2.41ms, predicted display time was too early
--   TRACE [u_pacing_app] Skipped! predicted frame was already too late for pacing
parseMonadoFrameSkipEventLine :: String -> Maybe (MonadoHudFrameSkipEventKind, Maybe Double)
parseMonadoFrameSkipEventLine line
  | "Compositor probably missed frame by " `List.isInfixOf` line =
      Just (MonadoHudDisplayMiss, parseDoubleAfter "missed frame by " line)
  | "Dropping old missed frame in favour for completed new frame" `List.isInfixOf` line =
      Just (MonadoHudOldFrameDrop, Nothing)
  | " missed by " `List.isInfixOf` line && "Frame " `List.isInfixOf` line =
      Just (MonadoHudPacingMiss, parseDoubleAfter "missed by " line)
  | "Skipped!" `List.isInfixOf` line =
      Just (MonadoHudPredictionSkip, Nothing)
  | otherwise = Nothing

-- Examples:
--   TRACE [u_pacing_app] Delivered frame 0.34ms late
--   TRACE [u_pacing_app] Delivered frame -1.20ms early
parseDeliveredFrameLine :: String -> Maybe PendingMonadoAppTiming
parseDeliveredFrameLine line =
  if "Delivered frame " `List.isInfixOf` line
    then
      Just
        PendingMonadoAppTiming
          { pendingAppDeliveredDeltaMs = parseDoubleAfter "Delivered frame " line
          , pendingAppDeliveredLate = " late" `List.isInfixOf` line
          , pendingAppPeriodMs = Nothing
          , pendingAppCpu = Nothing
          , pendingAppDraw = Nothing
          , pendingAppGpu = Nothing
          }
    else Nothing

-- Examples:
--   TRACE [u_pacing_app]     period: 11.111ms
--   TRACE [u_pacing_app]     cpu  o: 2.15ms n: 2.60ms
--   TRACE [u_pacing_app]     draw o: 3.40ms n: 4.10ms
--   TRACE [u_pacing_app]     gpu  o: 5.20ms n: 6.05ms
updatePendingAppTimingFromLine :: PendingMonadoAppTiming -> String -> PendingMonadoAppTiming
updatePendingAppTimingFromLine pending line
  | "period:" `List.isInfixOf` line =
      pending { pendingAppPeriodMs = parseDoubleAfter "period:" line }
  | "cpu  o:" `List.isInfixOf` line =
      pending { pendingAppCpu = parseOldNewPair line }
  | "draw o:" `List.isInfixOf` line =
      pending { pendingAppDraw = parseOldNewPair line }
  | "gpu  o:" `List.isInfixOf` line =
      pending { pendingAppGpu = parseOldNewPair line }
  | otherwise = pending

completePendingAppTiming :: PendingMonadoAppTiming -> Maybe MonadoAppTiming
completePendingAppTiming pending = do
  (cpuEstimate, cpuObserved) <- pendingAppCpu pending
  (drawEstimate, drawObserved) <- pendingAppDraw pending
  (gpuEstimate, gpuObserved) <- pendingAppGpu pending
  return
    MonadoAppTiming
      { monadoAppDeliveredDeltaMs = pendingAppDeliveredDeltaMs pending
      , monadoAppDeliveredLate = pendingAppDeliveredLate pending
      , monadoAppPeriodMs = pendingAppPeriodMs pending
      , monadoAppCpuEstimateMs = cpuEstimate
      , monadoAppCpuObservedMs = cpuObserved
      , monadoAppDrawEstimateMs = drawEstimate
      , monadoAppDrawObservedMs = drawObserved
      , monadoAppGpuEstimateMs = gpuEstimate
      , monadoAppGpuObservedMs = gpuObserved
      }

-- We accept both compositor timing block starts:
--
--   TRACE [pc_info] Got
--     Display-timing-aware pacer, followed by actual present/margin fields.
--
--   TRACE [log_measured_compositor_timing] Got measured compositor timing from compositor pacer
--     Fake compositor pacer fallback, followed by real wake/submit/gpu_end fields
--     but no actual_present_time_ns or present_margin_ns.
--
-- Field examples:
--   TRACE [u_pacing_compositor]     since_last_frame_ms: 11.11
--   TRACE [u_pacing_compositor]     when_woke_ns: 9066425898123456
--   TRACE [u_pacing_compositor]     desired_present_time_ns: 9066425910000000
--   TRACE [u_pacing_compositor]     actual_present_time_ns: 9066425916800000
--   TRACE [u_pacing_compositor]     earliest_present_time_ns: 9066425909200000
--   TRACE [u_pacing_compositor]     present_margin_ns: 4200000
startsMonadoCompositorTimingBlock :: String -> Bool
startsMonadoCompositorTimingBlock line =
  startsNewMonadoCompositorTimingBlock line
    || "when_woke_ns:" `List.isInfixOf` line
    || "when_submitted_ns:" `List.isInfixOf` line
    || "when_infoed_ns:" `List.isInfixOf` line
    || "since_last_frame_ms:" `List.isInfixOf` line
    || "desired_present_time_ns:" `List.isInfixOf` line
    || "gpu_end_ns:" `List.isInfixOf` line
    || "actual_present_time_ns:" `List.isInfixOf` line
    || "earliest_present_time_ns:" `List.isInfixOf` line
    || "present_margin_ns:" `List.isInfixOf` line

startsNewMonadoCompositorTimingBlock :: String -> Bool
startsNewMonadoCompositorTimingBlock line =
  "Got" `List.isInfixOf` line
    && ( "[pc_info]" `List.isInfixOf` line
           || "compositor pacer" `List.isInfixOf` line
       )

-- Field examples:
--   TRACE [u_pacing_compositor]     since_last_frame_ms: 11.11
--   TRACE [u_pacing_compositor]     when_woke_ns: 9066425898123456
--   TRACE [u_pacing_compositor]     when_submitted_ns: 9066425901567890
--   TRACE [u_pacing_compositor]     desired_present_time_ns: 9066425910000000
--   TRACE [u_pacing_compositor]     gpu_end_ns: 9066425905500000
--   TRACE [u_pacing_compositor]     actual_present_time_ns: 9066425916800000
--   TRACE [u_pacing_compositor]     earliest_present_time_ns: 9066425909200000
--   TRACE [u_pacing_compositor]     when_infoed_ns: 9066425919100000
--   TRACE [u_pacing_compositor]     present_margin_ns: 4200000
--   TRACE [u_pacing_compositor]     present_margin_ms: 4.20
updatePendingCompositorTimingFromLine :: PendingMonadoCompositorTiming -> String -> PendingMonadoCompositorTiming
updatePendingCompositorTimingFromLine pending line
  | "when_woke_ns:" `List.isInfixOf` line =
      pending { pendingCompositorWhenWokeNs = parseIntegerAfter "when_woke_ns:" line }
  | "when_submitted_ns:" `List.isInfixOf` line =
      pending { pendingCompositorWhenSubmittedNs = parseIntegerAfter "when_submitted_ns:" line }
  | "when_infoed_ns:" `List.isInfixOf` line =
      pending { pendingCompositorWhenInfoedNs = parseIntegerAfter "when_infoed_ns:" line }
  | "since_last_frame_ms:" `List.isInfixOf` line =
      pending { pendingCompositorSinceLastFrameMs = parseDoubleAfter "since_last_frame_ms:" line }
  | "desired_present_time_ns:" `List.isInfixOf` line =
      pending { pendingCompositorDesiredPresentNs = parseIntegerAfter "desired_present_time_ns:" line }
  | "gpu_end_ns:" `List.isInfixOf` line =
      pending { pendingCompositorGpuEndNs = parseIntegerAfter "gpu_end_ns:" line }
  | "actual_present_time_ns:" `List.isInfixOf` line =
      pending { pendingCompositorActualPresentNs = parseIntegerAfter "actual_present_time_ns:" line }
  | "earliest_present_time_ns:" `List.isInfixOf` line =
      pending { pendingCompositorEarliestPresentNs = parseIntegerAfter "earliest_present_time_ns:" line }
  | "present_margin_ns:" `List.isInfixOf` line =
      pending { pendingCompositorPresentMarginNs = parseIntegerAfter "present_margin_ns:" line }
  | "present_margin_ms:" `List.isInfixOf` line =
      pending { pendingCompositorPresentMarginMs = parseDoubleAfter "present_margin_ms:" line }
  | otherwise = pending

completePendingCompositorTiming :: PendingMonadoCompositorTiming -> Maybe MonadoCompositorTiming
completePendingCompositorTiming pending
  | hasActualMargin || hasMeasuredCompositorGpuDone =
      Just
        MonadoCompositorTiming
          { monadoCompositorWakeToSubmitMs = wakeToSubmit
          , monadoCompositorSubmitToInferredGpuDoneMs = submitToInferredGpuDone
          , monadoCompositorInferredGpuDoneToDesiredMs = inferredGpuDoneToDesired
          , monadoCompositorSubmitToActualPresentMs = submitToActualPresent
          , monadoCompositorWakeToActualPresentMs = wakeToActualPresent
          , monadoCompositorDesiredToActualPresentMs = desiredToActualPresent
          , monadoCompositorEarliestToActualPresentMs = earliestToActualPresent
          , monadoCompositorInfoAfterActualPresentMs = infoAfterActualPresent
          , monadoCompositorPresentMarginMs = pendingCompositorPresentMarginMs pending
          , monadoCompositorSinceLastFrameMs = pendingCompositorSinceLastFrameMs pending
          }
  | otherwise = Nothing
  where
    hasActualMargin = isJust (pendingCompositorPresentMarginMs pending)
    hasMeasuredCompositorGpuDone = isJust wakeToSubmit && isJust submitToInferredGpuDone
    wakeToSubmit =
      case (pendingCompositorWhenWokeNs pending, pendingCompositorWhenSubmittedNs pending) of
        (Just woke, Just submitted)
          | submitted >= woke -> Just $ fromIntegral (submitted - woke) / 1000000.0
        _ -> Nothing
    inferredGpuDoneNsFromPresentFeedback =
      case (pendingCompositorActualPresentNs pending, pendingCompositorPresentMarginNs pending) of
        (Just actual, Just margin) -> Just (actual - margin)
        _ -> Nothing
    gpuDoneNs =
      case pendingCompositorGpuEndNs pending of
        Just gpuEnd -> Just gpuEnd
        Nothing -> inferredGpuDoneNsFromPresentFeedback
    submitToInferredGpuDone =
      case (pendingCompositorWhenSubmittedNs pending, gpuDoneNs) of
        (Just submitted, Just gpuDone) -> positiveDeltaNsToMs submitted gpuDone
        _ -> Nothing
    inferredGpuDoneToDesired =
      case (gpuDoneNs, pendingCompositorDesiredPresentNs pending) of
        (Just gpuDone, Just desired) -> Just $ signedDeltaNsToMs gpuDone desired
        _ -> Nothing
    submitToActualPresent =
      case (pendingCompositorWhenSubmittedNs pending, pendingCompositorActualPresentNs pending) of
        (Just submitted, Just actual) -> positiveDeltaNsToMs submitted actual
        _ -> Nothing
    wakeToActualPresent =
      case (pendingCompositorWhenWokeNs pending, pendingCompositorActualPresentNs pending) of
        (Just woke, Just actual) -> positiveDeltaNsToMs woke actual
        _ -> Nothing
    desiredToActualPresent =
      case (pendingCompositorDesiredPresentNs pending, pendingCompositorActualPresentNs pending) of
        (Just desired, Just actual) -> Just $ signedDeltaNsToMs desired actual
        _ -> Nothing
    earliestToActualPresent =
      case (pendingCompositorEarliestPresentNs pending, pendingCompositorActualPresentNs pending) of
        (Just earliest, Just actual) -> Just $ signedDeltaNsToMs earliest actual
        _ -> Nothing
    infoAfterActualPresent =
      case (pendingCompositorActualPresentNs pending, pendingCompositorWhenInfoedNs pending) of
        (Just actual, Just infoed) -> Just $ signedDeltaNsToMs actual infoed
        _ -> Nothing

parseOldNewPair :: String -> Maybe (Double, Double)
parseOldNewPair line = do
  oldValue <- parseDoubleAfter "o:" line
  newValue <- parseDoubleAfter "n:" line
  return (oldValue, newValue)

parseDoubleAfter :: String -> String -> Maybe Double
parseDoubleAfter marker text =
  parseLeadingDouble =<< textAfter marker text

parseIntegerAfter :: String -> String -> Maybe Integer
parseIntegerAfter marker text =
  parseLeadingInteger =<< textAfter marker text

textAfter :: String -> String -> Maybe String
textAfter marker text =
  listToMaybe $
    mapMaybe (List.stripPrefix marker) (List.tails text)

parseLeadingDouble :: String -> Maybe Double
parseLeadingDouble =
  readMaybe . takeNumberToken . dropWhile (\c -> Char.isSpace c || c == '=')

parseLeadingInteger :: String -> Maybe Integer
parseLeadingInteger =
  readMaybe . takeIntegerToken . dropWhile (\c -> Char.isSpace c || c == '=')

takeNumberToken :: String -> String
takeNumberToken =
  takeWhile (\c -> Char.isDigit c || c == '.' || c == '-')

takeIntegerToken :: String -> String
takeIntegerToken =
  takeWhile (\c -> Char.isDigit c || c == '-')

positiveDeltaNsToMs :: Integer -> Integer -> Maybe Double
positiveDeltaNsToMs start end
  | end >= start = Just $ signedDeltaNsToMs start end
  | otherwise = Nothing

signedDeltaNsToMs :: Integer -> Integer -> Double
signedDeltaNsToMs start end =
  fromIntegral (end - start) / 1000000.0
