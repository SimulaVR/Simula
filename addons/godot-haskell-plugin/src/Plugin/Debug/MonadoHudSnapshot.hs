module Plugin.Debug.MonadoHudSnapshot where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Maybe
import Data.Time.Clock

import Plugin.Debug.MonadoHudState (trimMonadoHudState)
import Plugin.Debug.MonadoHudTypes

buildDebugMonadoHudSnapshot :: UTCTime -> FilePath -> MonadoHudState -> DebugMonadoHudSnapshot
buildDebugMonadoHudSnapshot now path state =
  DebugMonadoHudSnapshot
    { debugMonadoHudSnapshotUpdatedAt = now
    , debugMonadoHudSnapshotLogPath = path
    , debugMonadoHudSnapshotWindowSeconds = debugMonadoHudWindowSeconds
    , debugMonadoHudSnapshotDisplayMissCount = countKind MonadoHudDisplayMiss
    , debugMonadoHudSnapshotOldFrameDropCount = countKind MonadoHudOldFrameDrop
    , debugMonadoHudSnapshotPacingMissCount = countKind MonadoHudPacingMiss
    , debugMonadoHudSnapshotPredictionSkipCount = countKind MonadoHudPredictionSkip
    , debugMonadoHudSnapshotLatestDisplayMissMs = lastMaybe $ mapMaybe monadoHudFrameSkipEventMs displayMisses
    , debugMonadoHudSnapshotWorstDisplayMissWindowMs = maxMaybe $ mapMaybe monadoHudFrameSkipEventMs displayMisses
    , debugMonadoHudSnapshotWorstDisplayMissSessionMs = monadoHudStateWorstDisplayMissSessionMs state
    , debugMonadoHudSnapshotLatestAppTiming = monadoTimedValue <$> lastMaybe appSamples
    , debugMonadoHudSnapshotWorstAppObservedWindowMs = maxMaybe appObservedValues
    , debugMonadoHudSnapshotWorstAppObservedSessionMs = monadoHudStateWorstAppObservedSessionMs state
    , debugMonadoHudSnapshotAvgAppObservedMs = average appObservedValues
    , debugMonadoHudSnapshotWorstAppEstimateWindowMs = maxMaybe appEstimateValues
    , debugMonadoHudSnapshotWorstAppEstimateSessionMs = monadoHudStateWorstAppEstimateSessionMs state
    , debugMonadoHudSnapshotAvgAppEstimateMs = average appEstimateValues
    , debugMonadoHudSnapshotWorstAppCpuEstimateWindowMs = maxMaybe appCpuEstimateValues
    , debugMonadoHudSnapshotWorstAppCpuEstimateSessionMs = monadoHudStateWorstAppCpuEstimateSessionMs state
    , debugMonadoHudSnapshotAvgAppCpuEstimateMs = average appCpuEstimateValues
    , debugMonadoHudSnapshotWorstAppCpuObservedWindowMs = maxMaybe appCpuObservedValues
    , debugMonadoHudSnapshotWorstAppCpuObservedSessionMs = monadoHudStateWorstAppCpuObservedSessionMs state
    , debugMonadoHudSnapshotAvgAppCpuObservedMs = average appCpuObservedValues
    , debugMonadoHudSnapshotWorstAppDrawEstimateWindowMs = maxMaybe appDrawEstimateValues
    , debugMonadoHudSnapshotWorstAppDrawEstimateSessionMs = monadoHudStateWorstAppDrawEstimateSessionMs state
    , debugMonadoHudSnapshotAvgAppDrawEstimateMs = average appDrawEstimateValues
    , debugMonadoHudSnapshotWorstAppDrawObservedWindowMs = maxMaybe appDrawObservedValues
    , debugMonadoHudSnapshotWorstAppDrawObservedSessionMs = monadoHudStateWorstAppDrawObservedSessionMs state
    , debugMonadoHudSnapshotAvgAppDrawObservedMs = average appDrawObservedValues
    , debugMonadoHudSnapshotWorstAppGpuEstimateWindowMs = maxMaybe appGpuEstimateValues
    , debugMonadoHudSnapshotWorstAppGpuEstimateSessionMs = monadoHudStateWorstAppGpuEstimateSessionMs state
    , debugMonadoHudSnapshotAvgAppGpuEstimateMs = average appGpuEstimateValues
    , debugMonadoHudSnapshotWorstAppGpuObservedWindowMs = maxMaybe appGpuObservedValues
    , debugMonadoHudSnapshotWorstAppGpuObservedSessionMs = monadoHudStateWorstAppGpuObservedSessionMs state
    , debugMonadoHudSnapshotAvgAppGpuObservedMs = average appGpuObservedValues
    , debugMonadoHudSnapshotLatestCompositorTiming = monadoTimedValue <$> lastMaybe compositorSamples
    , debugMonadoHudSnapshotWorstCompositorWindowMs = maxMaybe compositorValues
    , debugMonadoHudSnapshotWorstCompositorSessionMs = monadoHudStateWorstCompositorSessionMs state
    , debugMonadoHudSnapshotAvgCompositorMs = average compositorValues
    , debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneWindowMs = maxMaybe compositorSubmitToInferredGpuDoneValues
    , debugMonadoHudSnapshotWorstCompositorSubmitToInferredGpuDoneSessionMs = monadoHudStateWorstCompositorSubmitToInferredGpuDoneSessionMs state
    , debugMonadoHudSnapshotAvgCompositorSubmitToInferredGpuDoneMs = average compositorSubmitToInferredGpuDoneValues
    , debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredWindowMs = minMaybe compositorInferredGpuDoneToDesiredValues
    , debugMonadoHudSnapshotWorstCompositorInferredGpuDoneToDesiredSessionMs = monadoHudStateWorstCompositorInferredGpuDoneToDesiredSessionMs state
    , debugMonadoHudSnapshotAvgCompositorInferredGpuDoneToDesiredMs = average compositorInferredGpuDoneToDesiredValues
    , debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentWindowMs = maxMaybe compositorSubmitToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorSubmitToActualPresentSessionMs = monadoHudStateWorstCompositorSubmitToActualPresentSessionMs state
    , debugMonadoHudSnapshotAvgCompositorSubmitToActualPresentMs = average compositorSubmitToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorWakeToActualPresentWindowMs = maxMaybe compositorWakeToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorWakeToActualPresentSessionMs = monadoHudStateWorstCompositorWakeToActualPresentSessionMs state
    , debugMonadoHudSnapshotAvgCompositorWakeToActualPresentMs = average compositorWakeToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentWindowMs = maxMaybe compositorDesiredToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorDesiredToActualPresentSessionMs = monadoHudStateWorstCompositorDesiredToActualPresentSessionMs state
    , debugMonadoHudSnapshotAvgCompositorDesiredToActualPresentMs = average compositorDesiredToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentWindowMs = maxMaybe compositorEarliestToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorEarliestToActualPresentSessionMs = monadoHudStateWorstCompositorEarliestToActualPresentSessionMs state
    , debugMonadoHudSnapshotAvgCompositorEarliestToActualPresentMs = average compositorEarliestToActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentWindowMs = maxMaybe compositorInfoAfterActualPresentValues
    , debugMonadoHudSnapshotWorstCompositorInfoAfterActualPresentSessionMs = monadoHudStateWorstCompositorInfoAfterActualPresentSessionMs state
    , debugMonadoHudSnapshotAvgCompositorInfoAfterActualPresentMs = average compositorInfoAfterActualPresentValues
    , debugMonadoHudSnapshotWorstPresentMarginWindowMs = minMaybe presentMarginValues
    , debugMonadoHudSnapshotWorstPresentMarginSessionMs = monadoHudStateWorstPresentMarginSessionMs state
    , debugMonadoHudSnapshotAvgPresentMarginMs = average presentMarginValues
    , debugMonadoHudSnapshotWorstCompositorSinceLastFrameWindowMs = maxMaybe compositorSinceLastFrameValues
    , debugMonadoHudSnapshotWorstCompositorSinceLastFrameSessionMs = monadoHudStateWorstCompositorSinceLastFrameSessionMs state
    , debugMonadoHudSnapshotAvgCompositorSinceLastFrameMs = average compositorSinceLastFrameValues
    , debugMonadoHudSnapshotFrameSkipEventBuckets = buildMonadoFrameSkipEventBuckets now frameSkipEvents
    , debugMonadoHudSnapshotErrors = List.take 8 (monadoHudStateErrors state)
    }
  where
    trimmed = trimMonadoHudState now state
    frameSkipEvents = Foldable.toList (monadoHudStateFrameSkipEvents trimmed)
    appSamples = Foldable.toList (monadoHudStateAppTimingSamples trimmed)
    compositorSamples = Foldable.toList (monadoHudStateCompositorTimingSamples trimmed)
    displayMisses = List.filter (\event -> monadoHudFrameSkipEventKind event == MonadoHudDisplayMiss) frameSkipEvents
    appObservedValues = fmap (monadoAppObservedTotalMs . monadoTimedValue) appSamples
    appEstimateValues = fmap (monadoAppEstimatedTotalMs . monadoTimedValue) appSamples
    appCpuEstimateValues = fmap (monadoAppCpuEstimateMs . monadoTimedValue) appSamples
    appCpuObservedValues = fmap (monadoAppCpuObservedMs . monadoTimedValue) appSamples
    appDrawEstimateValues = fmap (monadoAppDrawEstimateMs . monadoTimedValue) appSamples
    appDrawObservedValues = fmap (monadoAppDrawObservedMs . monadoTimedValue) appSamples
    appGpuEstimateValues = fmap (monadoAppGpuEstimateMs . monadoTimedValue) appSamples
    appGpuObservedValues = fmap (monadoAppGpuObservedMs . monadoTimedValue) appSamples
    compositorValues = mapMaybe (monadoCompositorWakeToSubmitMs . monadoTimedValue) compositorSamples
    compositorSubmitToInferredGpuDoneValues = mapMaybe (monadoCompositorSubmitToInferredGpuDoneMs . monadoTimedValue) compositorSamples
    compositorInferredGpuDoneToDesiredValues = mapMaybe (monadoCompositorInferredGpuDoneToDesiredMs . monadoTimedValue) compositorSamples
    compositorSubmitToActualPresentValues = mapMaybe (monadoCompositorSubmitToActualPresentMs . monadoTimedValue) compositorSamples
    compositorWakeToActualPresentValues = mapMaybe (monadoCompositorWakeToActualPresentMs . monadoTimedValue) compositorSamples
    compositorDesiredToActualPresentValues = mapMaybe (monadoCompositorDesiredToActualPresentMs . monadoTimedValue) compositorSamples
    compositorEarliestToActualPresentValues = mapMaybe (monadoCompositorEarliestToActualPresentMs . monadoTimedValue) compositorSamples
    compositorInfoAfterActualPresentValues = mapMaybe (monadoCompositorInfoAfterActualPresentMs . monadoTimedValue) compositorSamples
    presentMarginValues = mapMaybe (monadoCompositorPresentMarginMs . monadoTimedValue) compositorSamples
    compositorSinceLastFrameValues = mapMaybe (monadoCompositorSinceLastFrameMs . monadoTimedValue) compositorSamples
    countKind kind = List.length $ List.filter (\event -> monadoHudFrameSkipEventKind event == kind) frameSkipEvents

emptyDebugMonadoHudSnapshot :: UTCTime -> FilePath -> DebugMonadoHudSnapshot
emptyDebugMonadoHudSnapshot now path =
  buildDebugMonadoHudSnapshot now path emptyMonadoHudState

buildMonadoFrameSkipEventBuckets :: UTCTime -> [MonadoHudFrameSkipEvent] -> [MonadoFrameSkipEventBucket]
buildMonadoFrameSkipEventBuckets now frameSkipEvents =
  fmap snd $
    List.foldl' addEventToBucket emptyBuckets frameSkipEvents
  where
    bucketCount = debugMonadoHudFrameSkipEventBucketCount
    emptyBuckets =
      [ (index, MonadoFrameSkipEventBucket index 0 False False Nothing)
      | index <- [0 .. bucketCount - 1]
      ]
    windowStart = addUTCTime (negate debugMonadoHudWindowInSeconds) now
    bucketSeconds = debugMonadoHudWindowSeconds / fromIntegral bucketCount

    addEventToBucket buckets event =
      updateAt (bucketIndex event) (updateBucket event) buckets

    bucketIndex event =
      max 0 $
        min (bucketCount - 1) $
          floor (realToFrac (diffUTCTime (monadoHudFrameSkipEventTime event) windowStart) / bucketSeconds :: Double)

    updateBucket event bucket =
      bucket
        { monadoFrameSkipEventBucketEventCount = monadoFrameSkipEventBucketEventCount bucket + 1
        , monadoFrameSkipEventBucketDisplayMiss =
            monadoFrameSkipEventBucketDisplayMiss bucket
              || monadoHudFrameSkipEventKind event == MonadoHudDisplayMiss
              || monadoHudFrameSkipEventKind event == MonadoHudPacingMiss
        , monadoFrameSkipEventBucketOldDrop =
            monadoFrameSkipEventBucketOldDrop bucket
              || monadoHudFrameSkipEventKind event == MonadoHudOldFrameDrop
        , monadoFrameSkipEventBucketWorstMs =
            maxMaybe $
              catMaybes [monadoFrameSkipEventBucketWorstMs bucket, monadoHudFrameSkipEventMs event]
        }

updateAt :: Int -> (a -> a) -> [(Int, a)] -> [(Int, a)]
updateAt _ _ [] = []
updateAt wanted f ((index, value) : rest)
  | index == wanted = (index, f value) : rest
  | otherwise = (index, value) : updateAt wanted f rest

average :: [Double] -> Maybe Double
average [] = Nothing
average values = Just $ sum values / fromIntegral (List.length values)

maxMaybe :: [Double] -> Maybe Double
maxMaybe [] = Nothing
maxMaybe values = Just $ List.maximum values

minMaybe :: [Double] -> Maybe Double
minMaybe [] = Nothing
minMaybe values = Just $ List.minimum values

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (List.last xs)
