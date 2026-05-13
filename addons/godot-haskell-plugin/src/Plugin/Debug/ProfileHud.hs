{-# LANGUAGE TypeApplications #-}

module Plugin.Debug.ProfileHud where

import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Control.Exception.Safe (SomeException, try)
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.STM
import Data.Colour
import Data.Colour.SRGB.Linear
import qualified Data.List as List
import Data.Time.Clock
import Godot.Core.GodotVisualServer as G
import qualified Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import qualified Godot.Methods as G
import Linear
import Numeric (showFFloat)

import Plugin.Debug.ProfileHudTypes
import Plugin.Imports
import Plugin.Types

getDebugProfileHudSnapshot :: IO DebugProfileHudSnapshot
getDebugProfileHudSnapshot
  | not debugProfileHudEnabled = do
      now <- getCurrentTime
      return $ emptyDebugProfileHudSnapshot now
  | otherwise = do
      now <- getCurrentTime
      cached <- readTVarIO debugProfileHudSnapshotCacheVar
      case cached of
        Just (cachedAt, snapshot)
          | diffUTCTime now cachedAt < debugProfileHudSnapshotIntervalInSeconds ->
              return snapshot
        _ -> do
          state <- readTVarIO debugProfileHudStateVar
          let snapshot = buildDebugProfileHudSnapshot now state
          atomically $ writeTVar debugProfileHudSnapshotCacheVar (Just (now, snapshot))
          queueDebugProfileHudOutputs snapshot
          return snapshot

queueDebugProfileHudOutputs :: DebugProfileHudSnapshot -> IO ()
queueDebugProfileHudOutputs snapshot = do
  shouldQueue <- atomically $ do
    inFlight <- readTVar debugProfileHudOutputInFlightVar
    lastWrite <- readTVar debugProfileHudOutputLastWriteVar
    let now = debugProfileHudSnapshotUpdatedAt snapshot
    let oldEnough =
          case lastWrite of
            Nothing -> True
            Just lastWriteAt -> diffUTCTime now lastWriteAt >= debugProfileHudOutputIntervalInSeconds
    if inFlight || not oldEnough
      then return False
      else do
        writeTVar debugProfileHudOutputInFlightVar True
        writeTVar debugProfileHudOutputLastWriteVar (Just now)
        return True
  when shouldQueue $
    void $
      forkIO $
        writeDebugProfileHudOutputs snapshot
          `finally` atomically (writeTVar debugProfileHudOutputInFlightVar False)

writeDebugProfileHudOutputs :: DebugProfileHudSnapshot -> IO ()
writeDebugProfileHudOutputs snapshot = do
  _ <- try (writeFile debugProfileHudLivePath (formatDebugProfileHudLiveSnapshot snapshot)) :: IO (Either SomeException ())
  _ <- try (writeFile debugProfileHudRecentMissedFramesPath (formatDebugProfileHudRecentMissedFrames snapshot)) :: IO (Either SomeException ())
  return ()

formatDebugProfileHudLiveSnapshot :: DebugProfileHudSnapshot -> String
formatDebugProfileHudLiveSnapshot snapshot =
  List.unlines $
    [ "SIMULA_DEBUG_PROFILE_HUD live"
    , "updated: " ++ show (debugProfileHudSnapshotUpdatedAt snapshot)
    , "mode: slow-frame summary"
    , "budget_ms: " ++ formatMsValue (debugProfileHudSnapshotBudgetMs snapshot)
    , "window_s: " ++ formatSecondsValue (debugProfileHudSnapshotWindowSeconds snapshot)
    , "last_frame_ms: " ++ formatMaybeMs (debugProfileHudSnapshotLastFrameMs snapshot)
    , "worst_recent_frame_ms: " ++ formatMaybeMs (debugProfileHudSnapshotWorstFrameMs snapshot)
    , "missed_frames_last_window: "
        ++ show (debugProfileHudSnapshotSlowFrameCount snapshot)
        ++ " / "
        ++ show (debugProfileHudSnapshotFramesSeenInWindow snapshot)
    , "frame_strip: " ++ formatFrameBucketStrip (debugProfileHudSnapshotFrameBuckets snapshot)
    , ""
    , "Top culprits from retained slow frames:"
    , padRight 72 "function/path" ++ padLeft 9 "sum_ms" ++ padLeft 9 "avg_ms" ++ padLeft 9 "max_ms" ++ padLeft 8 "calls" ++ padLeft 8 "frames" ++ padLeft 8 "tag"
    , List.replicate 123 '-'
    ]
      ++ ( if List.null rows
             then ["No over-budget frames retained in the current window."]
             else fmap formatDebugProfileHudLiveRow rows
         )
      ++ formatWorstFrameSummary snapshot
      ++ formatProfilerErrors snapshot
  where
    rows = debugProfileHudVisibleRows snapshot

formatDebugProfileHudLiveRow :: FoldedRow -> String
formatDebugProfileHudLiveRow row =
  padRight 72 (fitText 72 $ formatFoldedPath row)
    ++ padLeft 9 (formatMsValue $ foldedSumMs row)
    ++ padLeft 9 (formatMsValue $ foldedAvgMs row)
    ++ padLeft 9 (formatMsValue $ foldedMaxMs row)
    ++ padLeft 8 (show $ foldedCalls row)
    ++ padLeft 8 (show $ foldedFrames row)
    ++ padLeft 8 (formatCulpritTag $ foldedTag row)

formatWorstFrameSummary :: DebugProfileHudSnapshot -> [String]
formatWorstFrameSummary snapshot =
  case debugProfileHudSnapshotWorstFrame snapshot of
    Nothing -> []
    Just worstFrame ->
      [ ""
      , "Worst frame:"
      , "frame_id="
          ++ show (frameId worstFrame)
          ++ " frame_ms="
          ++ formatMsValue (frameElapsedMs worstFrame)
          ++ " over_ms="
          ++ formatMsValue (max 0 $ frameElapsedMs worstFrame - debugProfileHudSnapshotBudgetMs snapshot)
      , "top_path=" ++ maybe "(none)" formatFoldedPath (firstMaybe $ debugProfileHudVisibleRowsFromRows $ buildFoldedRows [worstFrame])
      ]

formatProfilerErrors :: DebugProfileHudSnapshot -> [String]
formatProfilerErrors snapshot =
  case debugProfileHudSnapshotErrors snapshot of
    [] -> []
    errors ->
      [ ""
      , "Profiler errors:"
      ]
        ++ errors

formatDebugProfileHudRecentMissedFrames :: DebugProfileHudSnapshot -> String
formatDebugProfileHudRecentMissedFrames snapshot =
  List.intercalate "\n" $
    if List.null frames
      then ["No retained slow frames."]
      else fmap (formatSlowFrameEvidence snapshot) frames
  where
    frames = debugProfileHudSnapshotRetainedSlowFrames snapshot

formatSlowFrameEvidence :: DebugProfileHudSnapshot -> FrameProfile -> String
formatSlowFrameEvidence snapshot frame =
  List.unlines $
    [ List.replicate 80 '='
    , "slow frame " ++ show (frameId frame)
    , "started: " ++ show (frameStart frame)
    , "ended: " ++ show (frameEnd frame)
    , "frame_ms: " ++ formatMsValue frameMs
    , "budget_ms: " ++ formatMsValue (debugProfileHudSnapshotBudgetMs snapshot)
    , "over_ms: " ++ formatMsValue (max 0 $ frameMs - debugProfileHudSnapshotBudgetMs snapshot)
    , ""
    , "CALL TREE"
    ]
      ++ concatMap (formatScopeTree 0) (frameRootScopes frame)
      ++ [ ""
         , "FOLDED BY FUNCTION/PATH"
         , padRight 5 "rank" ++ padRight 72 "function/path" ++ padLeft 9 "sum_ms" ++ padLeft 9 "self_ms" ++ padLeft 9 "avg_ms" ++ padLeft 9 "max_ms" ++ padLeft 8 "calls" ++ padLeft 8 "tag"
         , List.replicate 129 '-'
         ]
      ++ fmap formatFoldedEvidenceRow (zip [1 :: Int ..] $ List.sortBy compareFoldedRows $ buildFoldedRows [frame])
  where
    frameMs = frameElapsedMs frame

formatScopeTree :: Int -> ClosedScope -> [String]
formatScopeTree indent scope =
  (indentText ++ padRight (max 1 $ 58 - indent) (fitText (max 1 $ 58 - indent) $ closedLabel scope)
    ++ " inc "
    ++ padLeft 7 (formatNominalMs $ closedInclusive scope)
    ++ " self "
    ++ padLeft 7 (formatNominalMs $ closedExclusive scope))
    : concatMap (formatScopeTree (indent + 2)) (closedChildren scope)
  where
    indentText = List.replicate indent ' '

formatFoldedEvidenceRow :: (Int, FoldedRow) -> String
formatFoldedEvidenceRow (rank, row) =
  padRight 5 (show rank)
    ++ padRight 72 (fitText 72 $ formatFoldedPath row)
    ++ padLeft 9 (formatMsValue $ foldedSumMs row)
    ++ padLeft 9 (formatMsValue $ foldedSelfMs row)
    ++ padLeft 9 (formatMsValue $ foldedAvgMs row)
    ++ padLeft 9 (formatMsValue $ foldedMaxMs row)
    ++ padLeft 8 (show $ foldedCalls row)
    ++ padLeft 8 (formatCulpritTag $ foldedTag row)

drawDebugHudProfileUsage :: CanvasBase -> DebugProfileHudSnapshot -> GodotDynamicFont -> Float -> Float -> Float -> Float -> IO ()
drawDebugHudProfileUsage cb snapshot debugFont left top availableWidth availableHeight =
  profileScope "Plugin.Debug.ProfileHud.drawDebugHudProfileUsage" $ do
    debugPutStrLn "Plugin.Debug.ProfileHud.drawDebugHudProfileUsage"
    G.set_size debugFont 16
    let fontAscent = 14
    let lineStep = 18
    let stripTop = top + lineStep + 3
    let stripHeight = 12
    let tableHeaderBaseline = top + fontAscent + lineStep * 2
    let firstRowBaseline = tableHeaderBaseline + lineStep
    let sumRight = left + availableWidth * 0.60
    let avgRight = left + availableWidth * 0.70
    let maxRight = left + availableWidth * 0.80
    let callsRight = left + availableWidth * 0.90
    let tagRight = left + availableWidth
    let pathWidth = max 1 (sumRight - left - 8)
    let numberWidth = max 1 (avgRight - sumRight - 8)
    let rowsAvailable = max 0 (floor ((availableHeight - 4 * lineStep) / lineStep) :: Int)
    headerColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
    neutralColor <- (toLowLevel $ (rgb 0.74 0.78 0.82) `withOpacity` 1.0) :: IO GodotColor
    hotColor <- (toLowLevel $ (rgb 1.0 0.42 0.24) `withOpacity` 1.0) :: IO GodotColor
    lineColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.18) :: IO GodotColor
    let headerBaseline = top + fontAscent
    drawText (formatHudHeader snapshot) headerColor left headerBaseline availableWidth
    drawFrameStrip stripTop stripHeight
    drawText "culprit path" headerColor left tableHeaderBaseline pathWidth
    drawRightAlignedText "sum" headerColor sumRight tableHeaderBaseline numberWidth
    drawRightAlignedText "avg" headerColor avgRight tableHeaderBaseline numberWidth
    drawRightAlignedText "max" headerColor maxRight tableHeaderBaseline numberWidth
    drawRightAlignedText "calls" headerColor callsRight tableHeaderBaseline numberWidth
    drawRightAlignedText "tag" headerColor tagRight tableHeaderBaseline numberWidth
    drawHorizontalLine lineColor (tableHeaderBaseline + 5)
    when (List.null $ debugProfileHudVisibleRows snapshot) $
      drawText "No over-budget frames retained in the current window" neutralColor left firstRowBaseline availableWidth
    forM_ (zip [0 :: Int ..] (take rowsAvailable $ debugProfileHudVisibleRows snapshot)) $ \(rowIndex, row) -> do
      let baseline = firstRowBaseline + lineStep * fromIntegral rowIndex
      let rowColor = if foldedTag row `elem` [CulpritHud, CulpritSpike] || foldedSumMs row >= debugProfileHudSnapshotBudgetMs snapshot then hotColor else neutralColor
      drawText (fitMiddle pathWidth $ formatFoldedPath row) rowColor left baseline pathWidth
      drawRightAlignedText (formatMsValue $ foldedSumMs row) rowColor sumRight baseline numberWidth
      drawRightAlignedText (formatMsValue $ foldedAvgMs row) rowColor avgRight baseline numberWidth
      drawRightAlignedText (formatMsValue $ foldedMaxMs row) rowColor maxRight baseline numberWidth
      drawRightAlignedText (show $ foldedCalls row) rowColor callsRight baseline numberWidth
      drawRightAlignedText (formatCulpritTag $ foldedTag row) rowColor tagRight baseline numberWidth
      drawHorizontalLine lineColor (baseline + 5)
  where
    drawFrameStrip :: Float -> Float -> IO ()
    drawFrameStrip stripTop stripHeight = do
      fastColor <- (toLowLevel $ (rgb 0.22 0.80 0.34) `withOpacity` 0.94) :: IO GodotColor
      nearColor <- (toLowLevel $ (rgb 1.0 0.76 0.18) `withOpacity` 0.98) :: IO GodotColor
      slowColor <- (toLowLevel $ (rgb 1.0 0.22 0.16) `withOpacity` 0.98) :: IO GodotColor
      severeColor <- (toLowLevel $ (rgb 0.62 0.02 0.02) `withOpacity` 1.0) :: IO GodotColor
      emptyColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.10) :: IO GodotColor
      let buckets = debugProfileHudSnapshotFrameBuckets snapshot
      let bucketCount = max 1 (List.length buckets)
      let gap = 1
      let bucketWidth = max 1 ((availableWidth - fromIntegral (bucketCount - 1) * gap) / fromIntegral bucketCount)
      forM_ buckets $ \bucket -> do
        let index = frameBucketIndex bucket
        let x = left + fromIntegral index * (bucketWidth + gap)
        let severity =
              if debugProfileHudSnapshotBudgetMs snapshot <= 0
                then 0
                else frameBucketMaxMs bucket / debugProfileHudSnapshotBudgetMs snapshot
        let heightFraction
              | frameBucketFrameCount bucket <= 0 = 0.25
              | frameBucketHasSlow bucket = min 1.0 (0.55 + 0.20 * realToFrac severity)
              | frameBucketHasNear bucket = 0.58
              | otherwise = 0.34
        let h = max 2 (stripHeight * heightFraction)
        let y = stripTop + stripHeight - h
        rect <- toLowLevel (V2 (V2 x y) (V2 bucketWidth h)) :: IO GodotRect2
        G.draw_rect cb rect (bucketColor emptyColor fastColor nearColor slowColor severeColor bucket) True 1.0 False

    bucketColor :: GodotColor -> GodotColor -> GodotColor -> GodotColor -> GodotColor -> FrameBucket -> GodotColor
    bucketColor emptyColor fastColor nearColor slowColor severeColor bucket
      | frameBucketFrameCount bucket <= 0 = emptyColor
      | frameBucketHasSlow bucket && frameBucketMaxMs bucket >= debugProfileHudSnapshotBudgetMs snapshot * 2.0 = severeColor
      | frameBucketHasSlow bucket = slowColor
      | frameBucketHasNear bucket = nearColor
      | otherwise = fastColor

    drawHorizontalLine :: GodotColor -> Float -> IO ()
    drawHorizontalLine color y = do
      start <- toLowLevel (V2 left y) :: IO GodotVector2
      end <- toLowLevel (V2 (left + availableWidth) y) :: IO GodotVector2
      G.draw_line cb start end color 1 False

    drawText :: String -> GodotColor -> Float -> Float -> Float -> IO ()
    drawText text color x baseline maxWidth =
      bracket
        (toLowLevel (pack text) :: IO GodotString)
        Api.godot_string_destroy
        (\textStr -> do
          renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
          G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr color (round maxWidth))

    drawRightAlignedText :: String -> GodotColor -> Float -> Float -> Float -> IO ()
    drawRightAlignedText text color right baseline maxWidth =
      bracket
        (toLowLevel (pack text) :: IO GodotString)
        Api.godot_string_destroy
        (\textStr -> do
          V2 textWidth _ <- G.get_string_size (safeCast debugFont :: GodotFont) textStr >>= fromLowLevel :: IO (V2 Float)
          let x = max left (right - textWidth)
          renderPosition <- toLowLevel (V2 x baseline) :: IO GodotVector2
          G.draw_string cb (safeCast debugFont :: GodotFont) renderPosition textStr color (round maxWidth))

formatHudHeader :: DebugProfileHudSnapshot -> String
formatHudHeader snapshot =
  "SIMULA_DEBUG_PROFILE_HUD budget="
    ++ formatMsValue (debugProfileHudSnapshotBudgetMs snapshot)
    ++ "ms window="
    ++ formatSecondsValue (debugProfileHudSnapshotWindowSeconds snapshot)
    ++ "s missed_frames="
    ++ show (debugProfileHudSnapshotSlowFrameCount snapshot)
    ++ "/"
    ++ show (debugProfileHudSnapshotFramesSeenInWindow snapshot)
    ++ " worst="
    ++ formatMaybeMs (debugProfileHudSnapshotWorstFrameMs snapshot)

formatMaybeMs :: Maybe Double -> String
formatMaybeMs Nothing = "n/a"
formatMaybeMs (Just value) = formatMsValue value

formatNominalMs :: NominalDiffTime -> String
formatNominalMs = formatMsValue . nominalDiffTimeToMs

formatMsValue :: Double -> String
formatMsValue value = showFFloat (Just 2) value ""

formatSecondsValue :: Double -> String
formatSecondsValue value =
  if abs (value - fromInteger (round value)) < 0.005
    then show (round value :: Int)
    else showFFloat (Just 2) value ""

padRight :: Int -> String -> String
padRight width text =
  text ++ List.replicate (max 0 (width - List.length text)) ' '

padLeft :: Int -> String -> String
padLeft width text =
  List.replicate (max 0 (width - List.length text)) ' ' ++ text

fitText :: Int -> String -> String
fitText maxChars text =
  if List.length text <= maxChars
    then text
    else List.take (max 1 $ maxChars - 3) text ++ "..."

fitMiddle :: Float -> String -> String
fitMiddle maxWidth text =
  let maxChars = max 4 (floor (maxWidth / 8) :: Int)
   in if List.length text <= maxChars
        then text
        else
          let leftChars = max 1 ((maxChars - 3) `div` 2)
              rightChars = max 1 (maxChars - 3 - leftChars)
           in List.take leftChars text ++ "..." ++ List.drop (List.length text - rightChars) text

firstMaybe :: [a] -> Maybe a
firstMaybe [] = Nothing
firstMaybe (x:_) = Just x
