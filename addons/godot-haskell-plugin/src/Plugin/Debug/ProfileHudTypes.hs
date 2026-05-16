module Plugin.Debug.ProfileHudTypes where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.STM
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Time.Clock
import System.Environment (lookupEnv)
import System.IO.Unsafe
import Text.Read (readMaybe)

import Plugin.Debug.HudTypes

debugProfileHudEnabled :: IO Bool
debugProfileHudEnabled =
  debugHudModeActive DebugHudProfile

debugProfileHudLivePath :: FilePath
debugProfileHudLivePath = "./HUD_profile_live.txt"

debugProfileHudRecentMissedFramesPath :: FilePath
debugProfileHudRecentMissedFramesPath = "./HUD_profile_recent_missed_frames.txt"

debugProfileHudHeight :: Int
debugProfileHudHeight = debugProfileHudHeightForRows debugProfileHudReservedRows

debugProfileHudReservedRows :: Int
debugProfileHudReservedRows = 10

debugProfileHudLineHeightPixels :: Int
debugProfileHudLineHeightPixels = 18

debugProfileHudFixedHeaderRows :: Int
debugProfileHudFixedHeaderRows = 4

debugProfileHudBottomPaddingPixels :: Int
debugProfileHudBottomPaddingPixels = 12

debugProfileHudHeightForRows :: Int -> Int
debugProfileHudHeightForRows rowCount =
  debugProfileHudLineHeightPixels * (rowCount + debugProfileHudFixedHeaderRows)
    + debugProfileHudBottomPaddingPixels

debugProfileHudBudgetMs :: Double
debugProfileHudBudgetMs = unsafePerformIO $ readDoubleEnv "SIMULA_DEBUG_PROFILE_HUD_BUDGET_MS" 16.67
{-# NOINLINE debugProfileHudBudgetMs #-}

debugProfileRoot :: Maybe String
debugProfileRoot = unsafePerformIO $ do
  maybeValue <- lookupEnv "SIMULA_DEBUG_PROFILE_ROOT"
  return $ maybeValue >>= normalizeEnvString
{-# NOINLINE debugProfileRoot #-}

-- When SIMULA_DEBUG_PROFILE_ROOT is passed, we have to retain all frames, not
-- just those which exceed our frame skipping threshold/budget.
debugProfileHudFrameRetentionMinimumMs :: Double
debugProfileHudFrameRetentionMinimumMs =
  case debugProfileRoot of
    Just _ -> 0.0 -- If SIMULA_DEBUG_PROFILE_ROOT is passed, retain all frames
    Nothing -> debugProfileHudBudgetMs

debugProfileHudBudgetInSeconds :: NominalDiffTime
debugProfileHudBudgetInSeconds = realToFrac (debugProfileHudBudgetMs / 1000.0)

debugProfileHudWindowSeconds :: Double
debugProfileHudWindowSeconds = unsafePerformIO $ readDoubleEnv "SIMULA_DEBUG_PROFILE_HUD_WINDOW_S" 10.0
{-# NOINLINE debugProfileHudWindowSeconds #-}

debugProfileHudSnapshotWindowInSeconds :: NominalDiffTime
debugProfileHudSnapshotWindowInSeconds = realToFrac debugProfileHudWindowSeconds

debugProfileHudSampleRetentionWindowInSeconds :: NominalDiffTime
debugProfileHudSampleRetentionWindowInSeconds = debugProfileHudSnapshotWindowInSeconds

-- The following throttles the performance of the frame profiling itself.
-- These do not decide whether a frame is timed; they instead decide how often retained
-- frames are summarized, and how much slow-frame call-tree evidence is retained
-- for HUD/file analysis.

-- Minimum time between rebuilding the cached DebugProfileHudSnapshot. A larger
-- value lowers HUD analysis cost but makes the on-screen profile table update
-- less often.
debugProfileHudSnapshotIntervalInSeconds :: NominalDiffTime
debugProfileHudSnapshotIntervalInSeconds = 2.0

-- Minimum time between writing HUD_profile_live.txt and
-- HUD_profile_recent_missed_frames.txt. A larger value lowers file-formatting
-- and write cost, but makes the text files less fresh.
debugProfileHudOutputIntervalInSeconds :: NominalDiffTime
debugProfileHudOutputIntervalInSeconds = 1.0

-- Number of time "frame buckets" used for the HUD's recent-frame strip. More buckets
-- give finer visual time resolution, but require more per-snapshot aggregation
-- and more draw calls.
debugProfileHudFrameStripBucketCount :: Int
debugProfileHudFrameStripBucketCount = 60

-- Maximum number of slow FrameProfile call trees retained for culprit analysis.
debugProfileHudMaxRetainedSlowFrames :: Int
debugProfileHudMaxRetainedSlowFrames = 64

-- Maximum number of top-level scopes retained inside one slow frame. If a frame
-- has more roots than this, keep the most expensive roots by inclusive time.
debugProfileHudMaxRootScopesPerFrame :: Int
debugProfileHudMaxRootScopesPerFrame = 128

-- Maximum number of direct child scopes retained below any one scope. If a scope
-- has more children than this, keep the most expensive children by inclusive time.
debugProfileHudMaxChildrenPerScope :: Int
debugProfileHudMaxChildrenPerScope = 48

-- Maximum retained call-tree depth. Scopes at this depth are kept, but their
-- children are dropped to stop very deep helper chains from dominating evidence.
debugProfileHudMaxScopeDepth :: Int
debugProfileHudMaxScopeDepth = 16

-- Fraction of the frame budget at which a frame-strip bucket is marked "near
-- budget" rather than comfortably fast.
debugProfileHudNearBudgetFactor :: Double
debugProfileHudNearBudgetFactor = 0.80

readDoubleEnv :: String -> Double -> IO Double
readDoubleEnv name fallback = do
  maybeValue <- lookupEnv name
  return $
    case maybeValue >>= readMaybe of
      Just value -> value
      Nothing -> fallback

normalizeEnvString :: String -> Maybe String
normalizeEnvString value =
  let trimmed = trim value
   in if List.null trimmed
        then Nothing
        else Just trimmed
  where
    trim :: String -> String
    trim =
      List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

type FrameId = Int
type ProfileStack = [OpenScope]

data OpenFrame = OpenFrame
  { openFrameId         :: FrameId
  , openFrameStart      :: UTCTime
  , openFrameRootScopes :: [ClosedScope]
  }

data FrameProfile = FrameProfile
  { frameId         :: FrameId
  , frameStart      :: UTCTime
  , frameEnd        :: UTCTime
  , frameRootScopes :: [ClosedScope]
  }
  deriving (Show)

data FrameSample = FrameSample
  { frameSampleId    :: FrameId
  , frameSampleStart :: UTCTime
  , frameSampleEnd   :: UTCTime
  , frameSampleMs    :: Double
  }
  deriving (Show)

data OpenScope = OpenScope
  { -- Human-readable name for the scope currently running.
    -- Example: "Plugin.CanvasBase._draw" or "getDepthFirstWlrSurfaces".
    openLabel :: String

    -- Wall-clock timestamp captured when this scope was entered.
    -- On exit, inclusive time is computed as:
    --   now - openStart
  , openStart :: UTCTime

    -- Sum of inclusive times of child scopes that already finished
    -- while this scope was still open.
    --
    -- This is what lets us compute exclusive/self time:
    --   exclusive = inclusive - openChildTime
  , openChildTime :: NominalDiffTime

    -- Completed child calls of this still-running scope.
    --
    -- If A calls B four times, then while A is open and those B calls
    -- finish, A's openChildren accumulates four ClosedScope values for B.
  , openChildren :: [ClosedScope]
  }
  deriving (Show)

data ClosedScope = ClosedScope
  { -- Human-readable name copied from the OpenScope when it exits.
    closedLabel :: String

    -- Total wall-clock time from entering this scope to exiting it.
    -- This includes child scopes.
  , closedInclusive :: NominalDiffTime

    -- Time spent directly in this scope, excluding child scopes.
    --
    -- Formula:
    --   closedExclusive = closedInclusive - sum(child.closedInclusive)
  , closedExclusive :: NominalDiffTime

    -- Finished child calls nested under this scope.
    --
    -- This is the actual tree edge:
    --   A has children [B, B, B, B]
  , closedChildren :: [ClosedScope]
  }
  deriving (Show)

data CulpritTag
  = CulpritCalls
  | CulpritSpike
  | CulpritRare
  | CulpritSelf
  | CulpritPhase
  | CulpritHud
  deriving (Eq, Ord, Show)

data FoldedRow = FoldedRow
  { foldedPath   :: [String]
  , foldedSumMs  :: Double
  , foldedSelfMs :: Double
  , foldedCalls  :: Int
  , foldedMaxMs  :: Double
  , foldedFrames :: Int
  , foldedTag    :: CulpritTag
  }
  deriving (Show)

data FrameBucket = FrameBucket
  { frameBucketIndex      :: Int
  , frameBucketFrameCount :: Int
  , frameBucketMaxMs      :: Double
  , frameBucketHasNear    :: Bool
  , frameBucketHasSlow    :: Bool
  }
  deriving (Show)

data DebugProfileHudSnapshot = DebugProfileHudSnapshot
  { debugProfileHudSnapshotUpdatedAt          :: UTCTime
  , debugProfileHudSnapshotBudgetMs           :: Double
  , debugProfileHudSnapshotFrameRetentionMinimumMs :: Double
  , debugProfileHudSnapshotWindowSeconds      :: Double
  , debugProfileHudSnapshotProfileRoot        :: Maybe String
  , debugProfileHudSnapshotRows               :: [FoldedRow]
  , debugProfileHudSnapshotFrameBuckets       :: [FrameBucket]
  , debugProfileHudSnapshotFramesSeenInWindow :: Int
  , debugProfileHudSnapshotSlowFrameCount     :: Int
  , debugProfileHudSnapshotLastFrameMs        :: Maybe Double
  , debugProfileHudSnapshotWorstFrameMs       :: Maybe Double
  , debugProfileHudSnapshotWorstFrame         :: Maybe FrameProfile
  , debugProfileHudSnapshotRetainedSlowFrames :: [FrameProfile]
  , debugProfileHudSnapshotErrors             :: [String]
  }
  deriving (Show)

debugProfileHudSnapshotCacheVar :: TVar (Maybe (UTCTime, DebugProfileHudSnapshot))
debugProfileHudSnapshotCacheVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugProfileHudSnapshotCacheVar #-}

debugProfileHudOutputLastWriteVar :: TVar (Maybe UTCTime)
debugProfileHudOutputLastWriteVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugProfileHudOutputLastWriteVar #-}

debugProfileHudOutputInFlightVar :: TVar Bool
debugProfileHudOutputInFlightVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE debugProfileHudOutputInFlightVar #-}

data DebugProfileHudState = DebugProfileHudState
  { profileCurrentFrame                 :: Maybe OpenFrame
  , profileFramesSeenInWindow           :: Int
  , retainedSlowFrames                  :: Seq FrameProfile
  , profileStacksByThread               :: M.Map ThreadId ProfileStack
  , profileNextFrameId                  :: FrameId
  , profilePendingRootScopes            :: [ClosedScope]
  , profileFrameHistory                 :: Seq FrameSample
  , debugProfileHudStateErrors          :: [String]
  }

debugProfileHudInitialState :: DebugProfileHudState
debugProfileHudInitialState =
  DebugProfileHudState
    { profileCurrentFrame = Nothing
    , profileFramesSeenInWindow = 0
    , retainedSlowFrames = Seq.empty
    , profileStacksByThread = M.empty
    , profileNextFrameId = 1
    , profilePendingRootScopes = []
    , profileFrameHistory = Seq.empty
    , debugProfileHudStateErrors = []
    }

debugProfileHudStateVar :: TVar DebugProfileHudState
debugProfileHudStateVar = unsafePerformIO $ newTVarIO debugProfileHudInitialState
{-# NOINLINE debugProfileHudStateVar #-}

profileScope :: String -> IO a -> IO a
profileScope label action = do
  enabled <- debugProfileHudEnabled
  if not enabled
    then action
    else bracket (profileEnterNow label) profileExitAfterEnter (const action)
  where
    profileExitAfterEnter entered =
      when entered profileExitNow

profileEnter :: String -> IO ()
profileEnter label = do
  enabled <- debugProfileHudEnabled
  when enabled $
    void $ profileEnterNow label

profileExit :: IO ()
profileExit =
  profileExitNow

profileEnterNow :: String -> IO Bool
profileEnterNow label = do
  threadId <- myThreadId
  shouldProfile <- shouldProfileScope threadId label
  if not shouldProfile
    then return False
    else do
      now <- getCurrentTime
      atomically $
        modifyTVar' debugProfileHudStateVar $
          pushOpenScope threadId (OpenScope label now 0 [])
      return True

shouldProfileScope :: ThreadId -> String -> IO Bool
shouldProfileScope threadId label =
  case debugProfileRoot of
    Nothing -> return True
    Just rootLabel
      | label == rootLabel -> return True
      | otherwise -> do
          state <- readTVarIO debugProfileHudStateVar
          return $
            not $
              List.null $
                M.findWithDefault [] threadId (profileStacksByThread state)

profileExitNow :: IO ()
profileExitNow = do
  now <- getCurrentTime
  threadId <- myThreadId
  atomically $
    modifyTVar' debugProfileHudStateVar $
      popOpenScope now threadId

profileFrameBoundary :: IO ()
profileFrameBoundary = do
  enabled <- debugProfileHudEnabled
  when enabled $ do
    now <- getCurrentTime
    atomically $
      modifyTVar' debugProfileHudStateVar $
        advanceProfileFrame now

buildDebugProfileHudSnapshot :: UTCTime -> DebugProfileHudState -> DebugProfileHudSnapshot
buildDebugProfileHudSnapshot now state =
  DebugProfileHudSnapshot
    { debugProfileHudSnapshotUpdatedAt = now
    , debugProfileHudSnapshotBudgetMs = debugProfileHudBudgetMs
    , debugProfileHudSnapshotFrameRetentionMinimumMs = debugProfileHudFrameRetentionMinimumMs
    , debugProfileHudSnapshotWindowSeconds = debugProfileHudWindowSeconds
    , debugProfileHudSnapshotProfileRoot = debugProfileRoot
    , debugProfileHudSnapshotRows = debugProfileHudVisibleRowsFromRows foldedRows
    , debugProfileHudSnapshotFrameBuckets = buildFrameBuckets now frameSamples
    , debugProfileHudSnapshotFramesSeenInWindow = List.length frameSamples
    , debugProfileHudSnapshotSlowFrameCount = List.length slowFrames
    , debugProfileHudSnapshotLastFrameMs = frameSampleMs <$> lastMaybe frameSamples
    , debugProfileHudSnapshotWorstFrameMs = frameElapsedMs <$> worstFrame
    , debugProfileHudSnapshotWorstFrame = worstFrame
    , debugProfileHudSnapshotRetainedSlowFrames = slowFrames
    , debugProfileHudSnapshotErrors = List.take 12 (debugProfileHudStateErrors state)
    }
  where
    frameHistory = trimFrameSamples now (profileFrameHistory state)
    retained = trimRetainedSlowFrames now (retainedSlowFrames state)
    frameSamples = Foldable.toList frameHistory
    slowFrames = Foldable.toList retained
    foldedRows = buildFoldedRows slowFrames
    worstFrame =
      case slowFrames of
        [] -> Nothing
        _ -> Just $ List.maximumBy compareFrameElapsed slowFrames

emptyDebugProfileHudSnapshot :: UTCTime -> DebugProfileHudSnapshot
emptyDebugProfileHudSnapshot now =
  DebugProfileHudSnapshot
    { debugProfileHudSnapshotUpdatedAt = now
    , debugProfileHudSnapshotBudgetMs = debugProfileHudBudgetMs
    , debugProfileHudSnapshotFrameRetentionMinimumMs = debugProfileHudFrameRetentionMinimumMs
    , debugProfileHudSnapshotWindowSeconds = debugProfileHudWindowSeconds
    , debugProfileHudSnapshotProfileRoot = debugProfileRoot
    , debugProfileHudSnapshotRows = []
    , debugProfileHudSnapshotFrameBuckets = buildFrameBuckets now []
    , debugProfileHudSnapshotFramesSeenInWindow = 0
    , debugProfileHudSnapshotSlowFrameCount = 0
    , debugProfileHudSnapshotLastFrameMs = Nothing
    , debugProfileHudSnapshotWorstFrameMs = Nothing
    , debugProfileHudSnapshotWorstFrame = Nothing
    , debugProfileHudSnapshotRetainedSlowFrames = []
    , debugProfileHudSnapshotErrors = []
    }

debugProfileHudVisibleRows :: DebugProfileHudSnapshot -> [FoldedRow]
debugProfileHudVisibleRows =
  debugProfileHudVisibleRowsFromRows . debugProfileHudSnapshotRows

debugProfileHudVisibleRowsFromRows :: [FoldedRow] -> [FoldedRow]
debugProfileHudVisibleRowsFromRows =
  List.take debugProfileHudReservedRows . List.sortBy compareFoldedRows

compareFoldedRows :: FoldedRow -> FoldedRow -> Ordering
compareFoldedRows a b =
  compare
    (foldedSumMs b, foldedSelfMs b, foldedMaxMs b, foldedCalls b)
    (foldedSumMs a, foldedSelfMs a, foldedMaxMs a, foldedCalls a)
    <> compare (foldedPath a) (foldedPath b)

buildFoldedRows :: [FrameProfile] -> [FoldedRow]
buildFoldedRows slowFrames =
  fmap (classifyFoldedRow (List.length slowFrames)) $
    M.elems $
      M.unionsWith mergeFoldedRows $
        fmap foldFrameProfile slowFrames

foldFrameProfile :: FrameProfile -> M.Map [String] FoldedRow
foldFrameProfile frame =
  M.map (\row -> row { foldedFrames = 1 }) rawRows
  where
    rawRows = M.unionsWith mergeFoldedRows $ fmap (foldScope []) (frameRootScopes frame)

foldScope :: [String] -> ClosedScope -> M.Map [String] FoldedRow
foldScope parentPath scope =
  M.unionsWith mergeFoldedRows (selfRow : childRows)
  where
    path = parentPath ++ [closedLabel scope]
    selfRow =
      M.singleton path $
        FoldedRow
          { foldedPath = path
          , foldedSumMs = nominalDiffTimeToMs (closedInclusive scope)
          , foldedSelfMs = nominalDiffTimeToMs (closedExclusive scope)
          , foldedCalls = 1
          , foldedMaxMs = nominalDiffTimeToMs (closedInclusive scope)
          , foldedFrames = 0
          , foldedTag = CulpritSelf
          }
    childRows = fmap (foldScope path) (closedChildren scope)

mergeFoldedRows :: FoldedRow -> FoldedRow -> FoldedRow
mergeFoldedRows a b =
  a
    { foldedSumMs = foldedSumMs a + foldedSumMs b
    , foldedSelfMs = foldedSelfMs a + foldedSelfMs b
    , foldedCalls = foldedCalls a + foldedCalls b
    , foldedMaxMs = max (foldedMaxMs a) (foldedMaxMs b)
    , foldedFrames = foldedFrames a + foldedFrames b
    }

classifyFoldedRow :: Int -> FoldedRow -> FoldedRow
classifyFoldedRow slowFrameCount row =
  row { foldedTag = tag }
  where
    avgMs = foldedAvgMs row
    selfFraction =
      if foldedSumMs row <= 0
        then 0
        else foldedSelfMs row / foldedSumMs row
    lowerPath = Char.toLower <$> formatFoldedPath row
    tag
      | "profilehud" `List.isInfixOf` lowerPath
          || "drawdebughudprofileusage" `List.isInfixOf` lowerPath = CulpritHud
      | foldedCalls row >= 4 && avgMs <= max 1.0 (debugProfileHudBudgetMs * 0.35) = CulpritCalls
      | foldedCalls row > 1 && foldedMaxMs row >= avgMs * 2.5 = CulpritSpike
      | foldedFrames row <= 1 && slowFrameCount >= 2 = CulpritRare
      | selfFraction <= 0.25 && foldedSumMs row >= debugProfileHudBudgetMs * 0.35 = CulpritPhase
      | selfFraction >= 0.70 = CulpritSelf
      | foldedMaxMs row >= debugProfileHudBudgetMs * 0.50 = CulpritSpike
      | otherwise = CulpritSelf

formatCulpritTag :: CulpritTag -> String
formatCulpritTag CulpritCalls = "calls"
formatCulpritTag CulpritSpike = "spike"
formatCulpritTag CulpritRare = "rare"
formatCulpritTag CulpritSelf = "self"
formatCulpritTag CulpritPhase = "phase"
formatCulpritTag CulpritHud = "hud"

formatFoldedPath :: FoldedRow -> String
formatFoldedPath = List.intercalate " > " . foldedPath

foldedLeafLabel :: FoldedRow -> String
foldedLeafLabel row =
  case foldedPath row of
    [] -> ""
    labels -> List.last labels

foldedAvgMs :: FoldedRow -> Double
foldedAvgMs row =
  if foldedCalls row <= 0
    then 0
    else foldedSumMs row / fromIntegral (foldedCalls row)

pushOpenScope :: ThreadId -> OpenScope -> DebugProfileHudState -> DebugProfileHudState
pushOpenScope threadId scope state =
  state
    { profileStacksByThread =
        M.insert threadId (scope : M.findWithDefault [] threadId (profileStacksByThread state)) (profileStacksByThread state)
    }

popOpenScope :: UTCTime -> ThreadId -> DebugProfileHudState -> DebugProfileHudState
popOpenScope now threadId state =
  case M.findWithDefault [] threadId (profileStacksByThread state) of
    childOpen : parentOpen : rest ->
      let childClosed = closeScope now childOpen
          parentOpen' =
            parentOpen
              { openChildTime = openChildTime parentOpen + closedInclusive childClosed
              , openChildren = childClosed : openChildren parentOpen
              }
       in state { profileStacksByThread = M.insert threadId (parentOpen' : rest) (profileStacksByThread state) }
    rootOpen : [] ->
      let rootClosed = closeScope now rootOpen
          stacks' = M.delete threadId (profileStacksByThread state)
       in attachRootScope rootClosed state { profileStacksByThread = stacks' }
    [] ->
      recordProfilerError "profileExit with empty stack" state

closeScope :: UTCTime -> OpenScope -> ClosedScope
closeScope now scope =
  ClosedScope
    { closedLabel = openLabel scope
    , closedInclusive = inclusive
    , closedExclusive = max 0 (inclusive - openChildTime scope)
    , closedChildren = List.reverse (openChildren scope)
    }
  where
    inclusive = max 0 (diffUTCTime now (openStart scope))

attachRootScope :: ClosedScope -> DebugProfileHudState -> DebugProfileHudState
attachRootScope rootClosed state =
  case profileCurrentFrame state of
    Just frame ->
      state
        { profileCurrentFrame =
            Just frame { openFrameRootScopes = rootClosed : openFrameRootScopes frame }
        }
    Nothing ->
      state
        { profilePendingRootScopes = List.take 128 (rootClosed : profilePendingRootScopes state)
        }

advanceProfileFrame :: UTCTime -> DebugProfileHudState -> DebugProfileHudState
advanceProfileFrame now state =
  startNextFrame now $
    case profileCurrentFrame state of
      Nothing -> state
      Just currentFrame -> appendCompletedFrame now (closeFrame now currentFrame) state

startNextFrame :: UTCTime -> DebugProfileHudState -> DebugProfileHudState
startNextFrame now state =
  state
    { profileCurrentFrame =
        Just
          OpenFrame
            { openFrameId = profileNextFrameId state
            , openFrameStart = now
            , openFrameRootScopes = profilePendingRootScopes state
            }
    , profileNextFrameId = profileNextFrameId state + 1
    , profilePendingRootScopes = []
    }

closeFrame :: UTCTime -> OpenFrame -> FrameProfile
closeFrame now frame =
  FrameProfile
    { frameId = openFrameId frame
    , frameStart = openFrameStart frame
    , frameEnd = now
    , frameRootScopes = List.reverse (openFrameRootScopes frame)
    }

appendCompletedFrame :: UTCTime -> FrameProfile -> DebugProfileHudState -> DebugProfileHudState
appendCompletedFrame now frame state =
  state
    { profileFrameHistory = frameHistory'
    , retainedSlowFrames = retainedSlowFrames'
    , profileFramesSeenInWindow = Seq.length frameHistory'
    }
  where
    sample = frameSampleFromProfile frame
    frameHistory' = trimFrameSamples now (profileFrameHistory state |> sample)
    retainedSlowFrames'
      | shouldRetainFrame frame sample =
          trimRetainedSlowFrames now (retainedSlowFrames state |> pruneFrameProfile frame)
      | otherwise =
          trimRetainedSlowFrames now (retainedSlowFrames state)

shouldRetainFrame :: FrameProfile -> FrameSample -> Bool
shouldRetainFrame frame sample =
  frameSampleMs sample > debugProfileHudFrameRetentionMinimumMs
    && ( case debugProfileRoot of
           Nothing -> True
           Just _ -> not (List.null $ frameRootScopes frame)
       )

pruneFrameProfile :: FrameProfile -> FrameProfile
pruneFrameProfile frame =
  frame
    { frameRootScopes =
        fmap (pruneClosedScope 0) $
          takeMostExpensiveScopes debugProfileHudMaxRootScopesPerFrame $
            frameRootScopes frame
    }

pruneClosedScope :: Int -> ClosedScope -> ClosedScope
pruneClosedScope depth scope =
  if depth >= debugProfileHudMaxScopeDepth
    then scope { closedChildren = [] }
    else
      scope
        { closedChildren =
            fmap (pruneClosedScope (depth + 1)) $
              takeMostExpensiveScopes debugProfileHudMaxChildrenPerScope $
                closedChildren scope
        }

takeMostExpensiveScopes :: Int -> [ClosedScope] -> [ClosedScope]
takeMostExpensiveScopes maxCount scopes
  | List.length scopes <= maxCount = scopes
  | otherwise =
      fmap snd $
        List.sortOn fst $
          List.take maxCount $
            List.sortBy compareScopeCostDesc $
              zip [0 :: Int ..] scopes
  where
    compareScopeCostDesc (_, scopeA) (_, scopeB) =
      compare (closedInclusive scopeB) (closedInclusive scopeA)

frameSampleFromProfile :: FrameProfile -> FrameSample
frameSampleFromProfile frame =
  FrameSample
    { frameSampleId = frameId frame
    , frameSampleStart = frameStart frame
    , frameSampleEnd = frameEnd frame
    , frameSampleMs = frameElapsedMs frame
    }

frameElapsedMs :: FrameProfile -> Double
frameElapsedMs frame =
  nominalDiffTimeToMs (diffUTCTime (frameEnd frame) (frameStart frame))

nominalDiffTimeToMs :: NominalDiffTime -> Double
nominalDiffTimeToMs seconds =
  realToFrac seconds * 1000.0

compareFrameElapsed :: FrameProfile -> FrameProfile -> Ordering
compareFrameElapsed a b =
  compare (frameElapsedMs a) (frameElapsedMs b)

trimFrameSamples :: UTCTime -> Seq FrameSample -> Seq FrameSample
trimFrameSamples now =
  Seq.dropWhileL (\sample -> diffUTCTime now (frameSampleEnd sample) > debugProfileHudSnapshotWindowInSeconds)

trimRetainedSlowFrames :: UTCTime -> Seq FrameProfile -> Seq FrameProfile
trimRetainedSlowFrames now frames =
  trimSeqToLast debugProfileHudMaxRetainedSlowFrames $
    Seq.dropWhileL (\frame -> diffUTCTime now (frameEnd frame) > debugProfileHudSnapshotWindowInSeconds) frames

trimSeqToLast :: Int -> Seq a -> Seq a
trimSeqToLast maxCount values =
  Seq.drop (max 0 (Seq.length values - maxCount)) values

buildFrameBuckets :: UTCTime -> [FrameSample] -> [FrameBucket]
buildFrameBuckets now frameSamples =
  M.elems $
    List.foldl' addFrameToBucket emptyBuckets frameSamples
  where
    bucketCount = debugProfileHudFrameStripBucketCount
    emptyBuckets =
      M.fromList
        [ (index, FrameBucket index 0 0 False False)
        | index <- [0 .. bucketCount - 1]
        ]
    windowStart = addUTCTime (negate debugProfileHudSnapshotWindowInSeconds) now
    bucketSeconds = debugProfileHudWindowSeconds / fromIntegral bucketCount

    addFrameToBucket :: M.Map Int FrameBucket -> FrameSample -> M.Map Int FrameBucket
    addFrameToBucket buckets sample =
      M.adjust (updateBucket sample) (bucketIndex sample) buckets

    bucketIndex :: FrameSample -> Int
    bucketIndex sample =
      max 0 $
        min (bucketCount - 1) $
          floor (realToFrac (diffUTCTime (frameSampleEnd sample) windowStart) / bucketSeconds :: Double)

    updateBucket :: FrameSample -> FrameBucket -> FrameBucket
    updateBucket sample bucket =
      bucket
        { frameBucketFrameCount = frameBucketFrameCount bucket + 1
        , frameBucketMaxMs = max (frameBucketMaxMs bucket) (frameSampleMs sample)
        , frameBucketHasNear = frameBucketHasNear bucket || frameSampleMs sample >= debugProfileHudBudgetMs * debugProfileHudNearBudgetFactor
        , frameBucketHasSlow = frameBucketHasSlow bucket || frameSampleMs sample > debugProfileHudBudgetMs
        }

formatFrameBucketStrip :: [FrameBucket] -> String
formatFrameBucketStrip =
  fmap frameBucketGlyph

frameBucketGlyph :: FrameBucket -> Char
frameBucketGlyph bucket
  | frameBucketFrameCount bucket <= 0 = ' '
  | frameBucketHasSlow bucket && frameBucketMaxMs bucket >= debugProfileHudBudgetMs * 2.0 = '#'
  | frameBucketHasSlow bucket = '!'
  | frameBucketHasNear bucket = '-'
  | otherwise = '.'

recordProfilerError :: String -> DebugProfileHudState -> DebugProfileHudState
recordProfilerError err state =
  state { debugProfileHudStateErrors = List.take 24 (err : debugProfileHudStateErrors state) }

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (List.last xs)
