{-# LANGUAGE TypeApplications #-}

module Plugin.Debug.MemoryHud where

import Control.Concurrent.STM.TVar
import Control.Exception (bracket, finally)
import Control.Exception.Safe (SomeException, try)
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.STM
import Data.Colour
import Data.Colour.SRGB.Linear
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import Data.Text (unpack)
import Data.Time.Clock
import Godot.Core.GodotVisualServer as G
import qualified Godot.Gdnative.Internal.Api as Api
import Godot.Gdnative.Types
import qualified Godot.Methods as G
import Linear
import System.IO.Unsafe

import Plugin.Debug.MemoryHudTypes
import Plugin.Imports
import Plugin.Types

debugMemoryHudOutputFilesInitialize :: IO ()
debugMemoryHudOutputFilesInitialize = do
  alreadyInitialized <- atomically $ do
    initialized <- readTVar debugMemoryHudOutputFilesInitializedVar
    unless initialized $
      writeTVar debugMemoryHudOutputFilesInitializedVar True
    return initialized
  unless alreadyInitialized $ do
    now <- getCurrentTime
    writeFile debugMemoryHudOutputPath $
      "SIMULA_DEBUG_MEMORY_HUD output\n"
        ++ "started: " ++ show now ++ "\n\n"
    writeFile debugMemoryHudLivePath $
      "SIMULA_DEBUG_MEMORY_HUD live\n"
        ++ "started: " ++ show now ++ "\n"

debugMemoryHudGlobalSampleCacheForDeltaComputation :: TVar (Maybe (UTCTime, [DebugMemoryHudSample], DebugMemoryHudSnapshot))
debugMemoryHudGlobalSampleCacheForDeltaComputation = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugMemoryHudGlobalSampleCacheForDeltaComputation #-}

appendDebugMemoryHudOutput :: String -> IO ()
appendDebugMemoryHudOutput msg = do
  debugMemoryHudOutputFilesInitialize
  now <- getCurrentTime
  let line = show now ++ " " ++ msg ++ "\n"
  _ <- try (appendFile debugMemoryHudOutputPath line) :: IO (Either SomeException ())
  return ()

getDebugMemoryHudSnapshot :: GodotSimulaServer -> IO DebugMemoryHudSnapshot
getDebugMemoryHudSnapshot gss = do
  enabled <- debugMemoryHudEnabled
  if not enabled
    then
      return $ DebugMemoryHudSnapshot 0 0 [] Nothing
    else do
      now <- getCurrentTime
      cached <- readTVarIO debugMemoryHudGlobalSampleCacheForDeltaComputation
      case cached of
        Just (sampleTime, _, snapshot)
          | diffUTCTime now sampleTime < debugMemoryHudSampleIntervalInSeconds -> return snapshot
        _ -> refreshDebugMemoryHudSnapshot gss now cached

refreshDebugMemoryHudSnapshot
  :: GodotSimulaServer
  -> UTCTime
  -> Maybe (UTCTime, [DebugMemoryHudSample], DebugMemoryHudSnapshot)
  -> IO DebugMemoryHudSnapshot
refreshDebugMemoryHudSnapshot gss now cached = do
  sampleResult <- try (sampleLiveObjectDictionary gss) :: IO (Either SomeException (M.Map String Int))
  let previousSamples =
        case cached of
          Just (_, samples, _) -> samples
          Nothing -> []
  snapshot <- case sampleResult of
    Left err -> do
      appendDebugMemoryHudOutput $ "sampler failed: " ++ show err
      let snapshot = DebugMemoryHudSnapshot
            { debugMemoryHudSnapshotTotal = 0
            , debugMemoryHudSnapshotTotalDelta = 0
            , debugMemoryHudSnapshotRows = []
            , debugMemoryHudSnapshotError = Just "ObjectDB sampler failed; see ./HUD_output.txt"
            }
      writeDebugMemoryHudLiveSnapshot now snapshot
      return snapshot
    Right currentObjectCountMap -> do
      let baselineObjectCountMap = selectDebugMemoryHudBaselineObjectCountMap now previousSamples currentObjectCountMap
      let snapshot = buildDebugMemoryHudSnapshot baselineObjectCountMap currentObjectCountMap
      let samples = trimDebugMemoryHudSamples now (previousSamples ++ [(now, currentObjectCountMap)])
      appendDebugMemoryHudOutput $
        "sample ok: live="
          ++ show (debugMemoryHudSnapshotTotal snapshot)
          ++ " delta="
          ++ show (debugMemoryHudSnapshotTotalDelta snapshot)
          ++ " rows="
          ++ show (debugMemoryHudLogRows currentObjectCountMap)
      writeDebugMemoryHudLiveSnapshot now snapshot
      atomically $ writeTVar debugMemoryHudGlobalSampleCacheForDeltaComputation (Just (now, samples, snapshot))
      return snapshot
  case sampleResult of
    Left _ -> atomically $ writeTVar debugMemoryHudGlobalSampleCacheForDeltaComputation (Just (now, previousSamples, snapshot))
    Right _ -> return ()
  return snapshot

selectDebugMemoryHudBaselineObjectCountMap :: UTCTime -> [DebugMemoryHudSample] -> M.Map String Int -> M.Map String Int
selectDebugMemoryHudBaselineObjectCountMap _ [] currentObjectCountMap = currentObjectCountMap
selectDebugMemoryHudBaselineObjectCountMap now samples _ =
  case List.filter (\(sampleTime, _) -> diffUTCTime now sampleTime >= debugMemoryHudDeltaIntervalInSeconds) samples of
    [] -> snd (Prelude.head samples)
    oldEnoughSamples -> snd (Prelude.last oldEnoughSamples)

trimDebugMemoryHudSamples :: UTCTime -> [DebugMemoryHudSample] -> [DebugMemoryHudSample]
trimDebugMemoryHudSamples now =
  List.filter (\(sampleTime, _) -> diffUTCTime now sampleTime <= debugMemoryHudSampleRetentionWindowInSeconds)

debugMemoryHudLogRows :: M.Map String Int -> [(String, Int)]
debugMemoryHudLogRows objectCountMap =
  fmap
    (\(className, count) -> (debugMemoryHudDisplayName className, count))
    ( List.take 32 sortedRows ++
        List.filter
          (\(className, _) ->
            "script:" `List.isPrefixOf` className
              && not (className `List.elem` fmap fst (List.take 32 sortedRows)))
          sortedRows
    )
  where
    sortedRows :: [(String, Int)]
    sortedRows =
      List.sortBy
        (\(_, countA) (_, countB) -> compare countB countA)
        (M.toList objectCountMap)

writeDebugMemoryHudLiveSnapshot :: UTCTime -> DebugMemoryHudSnapshot -> IO ()
writeDebugMemoryHudLiveSnapshot now snapshot = do
  debugMemoryHudOutputFilesInitialize
  let contents =
        List.unlines $
          [ "SIMULA_DEBUG_MEMORY_HUD live"
          , "updated: " ++ show now
          , ""
          , padRight 46 "ObjectDB live objects" ++ padLeft 10 "count" ++ padLeft 10 "+/-5s"
          , padRight 46 "Total" ++ padLeft 10 (show $ debugMemoryHudSnapshotTotal snapshot) ++ padLeft 10 (formatDebugMemoryHudDelta $ debugMemoryHudSnapshotTotalDelta snapshot)
          , List.replicate 66 '-'
          ]
            ++ case debugMemoryHudSnapshotError snapshot of
              Just err -> [err]
              Nothing -> fmap formatDebugMemoryHudLiveRow (debugMemoryHudSnapshotRows snapshot)
  _ <- try (writeFile debugMemoryHudLivePath contents) :: IO (Either SomeException ())
  return ()

drawDebugHudMemoryUsage :: CanvasBase -> DebugMemoryHudSnapshot -> GodotDynamicFont -> Float -> Float -> Float -> Float -> IO ()
drawDebugHudMemoryUsage cb snapshot debugFont left top availableWidth availableHeight = do
  debugPutStrLn "Plugin.Debug.MemoryHud.drawDebugHudMemoryUsage"
  G.set_size debugFont 16
  let fontAscent = 14
  let lineStep = 18
  let countRight = left + availableWidth * 0.76
  let deltaRight = left + availableWidth
  let typeWidth = max 1 (countRight - left - 8)
  let countWidth = max 1 (deltaRight - countRight - 8)
  let visibleRows = debugMemoryHudVisibleRows snapshot
  let recentlyChangedRows = debugMemoryRecentlyChangedRows snapshot
  let rowsAvailable = max 0 (floor ((availableHeight - 2 * lineStep) / lineStep) :: Int)
  headerColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 1.0) :: IO GodotColor
  neutralColor <- (toLowLevel $ (rgb 0.74 0.78 0.82) `withOpacity` 1.0) :: IO GodotColor
  redColor <- (toLowLevel $ (rgb 1.0 0.25 0.18) `withOpacity` 1.0) :: IO GodotColor
  greenColor <- (toLowLevel $ (rgb 0.26 0.95 0.45) `withOpacity` 1.0) :: IO GodotColor
  lineColor <- (toLowLevel $ (rgb 1.0 1.0 1.0) `withOpacity` 0.18) :: IO GodotColor
  let headerBaseline = top + fontAscent
  drawText "ObjectDB recent activity" headerColor left headerBaseline typeWidth
  drawRightAlignedText "count" headerColor countRight headerBaseline countWidth
  drawRightAlignedText "+/-5s" headerColor deltaRight headerBaseline countWidth
  drawHorizontalLine lineColor (headerBaseline + 5)
  case debugMemoryHudSnapshotError snapshot of
    Just err -> drawText err redColor left (headerBaseline + lineStep) availableWidth
    Nothing -> do
      let totalBaseline = headerBaseline + lineStep
      drawText "Total" headerColor left totalBaseline typeWidth
      drawRightAlignedText (show $ debugMemoryHudSnapshotTotal snapshot) headerColor countRight totalBaseline countWidth
      drawRightAlignedDelta redColor greenColor neutralColor deltaRight countWidth (debugMemoryHudSnapshotTotalDelta snapshot) totalBaseline
      drawHorizontalLine lineColor (totalBaseline + 5)
      when (List.null recentlyChangedRows) $
        drawText "No recent object activity" neutralColor left (headerBaseline + lineStep * 2) availableWidth
      forM_ (zip [0 :: Int ..] (take rowsAvailable visibleRows)) $ \(rowIndex, row) -> do
        let baseline = headerBaseline + lineStep * fromIntegral (rowIndex + 2)
        drawText (fitClassName typeWidth $ debugMemoryHudRowClassName row) neutralColor left baseline typeWidth
        drawRightAlignedText (show $ debugMemoryHudRowCount row) neutralColor countRight baseline countWidth
        drawRightAlignedDelta redColor greenColor neutralColor deltaRight countWidth (debugMemoryHudRowDelta row) baseline
        drawHorizontalLine lineColor (baseline + 5)
  where
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

    drawRightAlignedDelta :: GodotColor -> GodotColor -> GodotColor -> Float -> Float -> Int -> Float -> IO ()
    drawRightAlignedDelta red green neutral right maxWidth delta baseline =
      drawRightAlignedText (formatDelta delta) (deltaColor red green neutral delta) right baseline maxWidth

    deltaColor :: GodotColor -> GodotColor -> GodotColor -> Int -> GodotColor
    deltaColor red green neutral delta
      | delta > 0 = red
      | delta < 0 = green
      | otherwise = neutral

    formatDelta :: Int -> String
    formatDelta delta
      | delta > 0 = "+" ++ show delta
      | otherwise = show delta

    fitClassName :: Float -> String -> String
    fitClassName maxWidth className =
      let maxChars = max 4 (floor (maxWidth / 8) :: Int)
      in if List.length className <= maxChars
        then className
        else List.take (maxChars - 3) className ++ "..."

formatDebugMemoryHudLiveRow :: DebugMemoryHudRow -> String
formatDebugMemoryHudLiveRow row =
  padRight 46 (debugMemoryHudRowClassName row)
    ++ padLeft 10 (show $ debugMemoryHudRowCount row)
    ++ padLeft 10 (formatDebugMemoryHudDelta $ debugMemoryHudRowDelta row)

formatDebugMemoryHudDelta :: Int -> String
formatDebugMemoryHudDelta delta
  | delta > 0 = "+" ++ show delta
  | otherwise = show delta

padRight :: Int -> String -> String
padRight width text =
  text ++ List.replicate (max 0 (width - List.length text)) ' '

padLeft :: Int -> String -> String
padLeft width text =
  List.replicate (max 0 (width - List.length text)) ' ' ++ text

sampleLiveObjectDictionary :: GodotSimulaServer -> IO (M.Map String Int)
sampleLiveObjectDictionary gss = do
  wlrBackend <- readTVarIO (gss ^. gssWlrBackend)
  backendClass <- objectClassName (safeCast wlrBackend :: GodotObject)
  appendDebugMemoryHudOutput $
    "sampling backend_class="
      ++ show backendClass
      ++ " using_get_live_object_dictionary=True"
  sampleDictionary <- G.get_live_object_dictionary wlrBackend
  finally
    (godotDictionaryToStringIntMap sampleDictionary)
    (Api.godot_dictionary_destroy sampleDictionary)

objectClassName :: GodotObject -> IO String
objectClassName object = do
  className <- G.get_class object
  classNameText <- fromLowLevel className `finally` Api.godot_string_destroy className
  return (unpack classNameText)

godotDictionaryToStringIntMap :: GodotDictionary -> IO (M.Map String Int)
godotDictionaryToStringIntMap dictionary = do
  keysArray <- Api.godot_dictionary_keys dictionary
  finally
    (do
      keyVariants <- fromGodotArray keysArray
      entries <- finally
        (mapM (readStringIntDictionaryEntry dictionary) keyVariants)
        (mapM_ Api.godot_variant_destroy keyVariants)
      return $ M.fromListWith (+) (Maybe.mapMaybe id entries))
    (Api.godot_array_destroy keysArray)

readStringIntDictionaryEntry :: GodotDictionary -> GodotVariant -> IO (Maybe (String, Int))
readStringIntDictionaryEntry dictionary keyVariant = do
  entryResult <- try (do
    keyString <- fromGodotVariant keyVariant :: IO GodotString
    key <- fromLowLevel keyString `finally` Api.godot_string_destroy keyString
    valueVariant <- Api.godot_dictionary_get dictionary keyVariant
    value <- (fromGodotVariant valueVariant :: IO Int) `finally` Api.godot_variant_destroy valueVariant
    return (unpack key, value)) :: IO (Either SomeException (String, Int))
  case entryResult of
    Right entry -> return $ Just entry
    Left _ -> return Nothing

debugMemoryHudDisplayName :: String -> String
debugMemoryHudDisplayName className
  | "script:" `List.isPrefixOf` className = List.drop (List.length ("script:" :: String)) className
  | "type:" `List.isPrefixOf` className = List.drop (List.length ("type:" :: String)) className
  | "parent:" `List.isPrefixOf` className = "  " ++ List.drop (List.length ("parent:" :: String)) className
  | otherwise = className

buildDebugMemoryHudSnapshot :: M.Map String Int -> M.Map String Int -> DebugMemoryHudSnapshot
buildDebugMemoryHudSnapshot baselineObjectCountMap currentObjectCountMap =
  DebugMemoryHudSnapshot
    { debugMemoryHudSnapshotTotal = total
    , debugMemoryHudSnapshotTotalDelta = total - baselineTotal
    , debugMemoryHudSnapshotRows = selectedRows
    , debugMemoryHudSnapshotError = Nothing
    }
  where
    total :: Int
    total = liveObjectCount currentObjectCountMap

    baselineTotal :: Int
    baselineTotal = liveObjectCount baselineObjectCountMap

    classNames :: [String]
    classNames = List.nub (M.keys currentObjectCountMap ++ M.keys baselineObjectCountMap)

    rows :: [DebugMemoryHudRow]
    rows =
      List.filter
        (\row ->
          not (debugMemoryHudRawClassNameIsHidden row)
            && (debugMemoryHudRowCount row /= 0 || debugMemoryHudRowDelta row /= 0))
        (fmap rowFor classNames)

    rowFor :: String -> DebugMemoryHudRow
    rowFor className =
      let currentObjectCount = M.findWithDefault 0 className currentObjectCountMap
          baselineObjectCount = M.findWithDefault 0 className baselineObjectCountMap
      in DebugMemoryHudRow (debugMemoryHudDisplayName className) currentObjectCount (currentObjectCount - baselineObjectCount)

    selectedRows :: [DebugMemoryHudRow]
    selectedRows =
      List.sortBy compareRowsByCount rows

compareRowsByCount :: DebugMemoryHudRow -> DebugMemoryHudRow -> Ordering
compareRowsByCount a b =
  compare
    (debugMemoryHudRowCount b, abs $ debugMemoryHudRowDelta b)
    (debugMemoryHudRowCount a, abs $ debugMemoryHudRowDelta a)
    <> compare (debugMemoryHudRowClassName a) (debugMemoryHudRowClassName b)

debugMemoryHudRawClassNameIsHidden :: DebugMemoryHudRow -> Bool
debugMemoryHudRawClassNameIsHidden row =
  debugMemoryHudRowClassName row `List.elem` debugMemoryHudHiddenRows

liveObjectCount :: M.Map String Int -> Int
liveObjectCount objectCountMap =
  M.findWithDefault (sum $ M.elems objectCountMap) "parent:Object" objectCountMap

debugMemoryHudHiddenRows :: [String]
debugMemoryHudHiddenRows = ["  Object", "  Reference"]
