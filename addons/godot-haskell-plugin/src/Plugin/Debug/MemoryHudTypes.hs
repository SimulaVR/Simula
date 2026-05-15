module Plugin.Debug.MemoryHudTypes where

import Control.Concurrent.STM.TVar
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Time.Clock
import System.IO.Unsafe

import Plugin.Debug.HudTypes

debugMemoryHudEnabled :: IO Bool
debugMemoryHudEnabled =
  debugHudModeActive DebugHudMemory

debugMemoryHudOutputPath :: FilePath
debugMemoryHudOutputPath = "./HUD_output.txt"

debugMemoryHudLivePath :: FilePath
debugMemoryHudLivePath = "./HUD_memory_live.txt"

debugMemoryHudOutputFilesInitializedVar :: TVar Bool
debugMemoryHudOutputFilesInitializedVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE debugMemoryHudOutputFilesInitializedVar #-}

debugMemoryHudHeight :: Int
debugMemoryHudHeight = debugMemoryHudHeightForRows debugMemoryHudReservedRows

debugMemoryHudReservedRows :: Int
debugMemoryHudReservedRows = 32

debugMemoryHudLineHeightPixels :: Int
debugMemoryHudLineHeightPixels = 18

debugMemoryHudFixedHeaderAndTotalRows :: Int
debugMemoryHudFixedHeaderAndTotalRows = 2

debugMemoryHudBottomPaddingPixels :: Int
debugMemoryHudBottomPaddingPixels = 12

debugMemoryHudHeightForRows :: Int -> Int
debugMemoryHudHeightForRows rowCount =
  debugMemoryHudLineHeightPixels * (rowCount + debugMemoryHudFixedHeaderAndTotalRows)
    + debugMemoryHudBottomPaddingPixels

debugMemoryHudSampleIntervalInSeconds :: NominalDiffTime
debugMemoryHudSampleIntervalInSeconds = 1

debugMemoryHudDeltaIntervalInSeconds :: NominalDiffTime
debugMemoryHudDeltaIntervalInSeconds = 5

debugMemoryHudSampleRetentionWindowInSeconds :: NominalDiffTime
debugMemoryHudSampleRetentionWindowInSeconds = 7

data DebugMemoryHudRow = DebugMemoryHudRow
  { debugMemoryHudRowClassName :: String
  , debugMemoryHudRowCount     :: Int
  , debugMemoryHudRowDelta     :: Int
  }

data DebugMemoryHudSnapshot = DebugMemoryHudSnapshot
  { debugMemoryHudSnapshotTotal      :: Int
  , debugMemoryHudSnapshotTotalDelta :: Int
  , debugMemoryHudSnapshotRows       :: [DebugMemoryHudRow]
  , debugMemoryHudSnapshotError      :: Maybe String
  }

type DebugMemoryHudSample = (UTCTime, M.Map String Int)

debugMemoryRecentlyChangedRows :: DebugMemoryHudSnapshot -> [DebugMemoryHudRow]
debugMemoryRecentlyChangedRows =
  List.filter ((/= 0) . debugMemoryHudRowDelta) . debugMemoryHudSnapshotRows

debugMemoryHudVisibleRows :: DebugMemoryHudSnapshot -> [DebugMemoryHudRow]
debugMemoryHudVisibleRows =
  List.take debugMemoryHudReservedRows . List.sortBy compareRowsByActivity . debugMemoryRecentlyChangedRows

compareRowsByActivity :: DebugMemoryHudRow -> DebugMemoryHudRow -> Ordering
compareRowsByActivity a b =
  compare
    (abs $ debugMemoryHudRowDelta b, debugMemoryHudRowCount b)
    (abs $ debugMemoryHudRowDelta a, debugMemoryHudRowCount a)
    <> compare (debugMemoryHudRowClassName a) (debugMemoryHudRowClassName b)
