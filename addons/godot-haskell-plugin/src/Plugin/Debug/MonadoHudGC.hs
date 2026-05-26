module Plugin.Debug.MonadoHudGC where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import GHC.Stats
import System.IO.Unsafe

import Plugin.Debug.MonadoHudTypes

debugMonadoHudGhcGCFrameTimingActiveVar :: TVar Bool
debugMonadoHudGhcGCFrameTimingActiveVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE debugMonadoHudGhcGCFrameTimingActiveVar #-}

debugMonadoHudPreviousGhcGCElapsedNsVar :: TVar (Maybe Integer)
debugMonadoHudPreviousGhcGCElapsedNsVar = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE debugMonadoHudPreviousGhcGCElapsedNsVar #-}

debugMonadoHudPendingGhcGCFrameTimeSamplesVar :: TVar (Seq Double)
debugMonadoHudPendingGhcGCFrameTimeSamplesVar = unsafePerformIO $ newTVarIO Seq.empty
{-# NOINLINE debugMonadoHudPendingGhcGCFrameTimeSamplesVar #-}

setDebugMonadoHudGhcGCFrameTimingActive :: Bool -> IO ()
setDebugMonadoHudGhcGCFrameTimingActive active =
  atomically $ do
    writeTVar debugMonadoHudGhcGCFrameTimingActiveVar active
    writeTVar debugMonadoHudPreviousGhcGCElapsedNsVar Nothing
    writeTVar debugMonadoHudPendingGhcGCFrameTimeSamplesVar Seq.empty

resetDebugMonadoHudGhcGCFrameTiming :: IO ()
resetDebugMonadoHudGhcGCFrameTiming =
  atomically $ do
    writeTVar debugMonadoHudPreviousGhcGCElapsedNsVar Nothing
    writeTVar debugMonadoHudPendingGhcGCFrameTimeSamplesVar Seq.empty

ghcGCFrameTimingStatsEnabled :: IO Bool
ghcGCFrameTimingStatsEnabled =
  getRTSStatsEnabled

recordDebugMonadoHudGhcGCFrame :: IO ()
recordDebugMonadoHudGhcGCFrame = do
  active <- readTVarIO debugMonadoHudGhcGCFrameTimingActiveVar
  if not active
    then return ()
    else do
      enabled <- ghcGCFrameTimingStatsEnabled
      if not enabled
        then return ()
        else do
          stats <- getRTSStats
          let currentGCElapsedNs = toInteger (gc_elapsed_ns stats)
          atomically $ do
            previousGCElapsedNs <- readTVar debugMonadoHudPreviousGhcGCElapsedNsVar
            writeTVar debugMonadoHudPreviousGhcGCElapsedNsVar (Just currentGCElapsedNs)
            let gcFrameMs =
                  case previousGCElapsedNs of
                    Nothing -> 0
                    Just previous
                      | currentGCElapsedNs >= previous ->
                          fromIntegral (currentGCElapsedNs - previous) / 1000000.0
                    Just _ -> 0
            modifyTVar' debugMonadoHudPendingGhcGCFrameTimeSamplesVar (appendBoundedGhcGCFrameTimeSample gcFrameMs)

drainDebugMonadoHudGhcGCFrameTimingSamples :: [OpenXRFrameTimingSample] -> IO [GhcGCFrameTimingSample]
drainDebugMonadoHudGhcGCFrameTimingSamples openXRFrameTimingSamples = do
  enabled <- ghcGCFrameTimingStatsEnabled
  if not enabled
    then return []
    else
      atomically drainPendingGhcGCFrameTimeSamples >>= \ghcGCFrameTimeSamples ->
        return $ alignGhcGCSamplesWithOpenXRFrameTimingSamples ghcGCFrameTimeSamples openXRFrameTimingSamples

appendBoundedGhcGCFrameTimeSample :: Double -> Seq Double -> Seq Double
appendBoundedGhcGCFrameTimeSample sample samples =
  let appended = samples |> sample
   in trimSeqToMax debugMonadoHudMaxTimingSamples appended

drainPendingGhcGCFrameTimeSamples :: STM [Double]
drainPendingGhcGCFrameTimeSamples = do
  samples <- readTVar debugMonadoHudPendingGhcGCFrameTimeSamplesVar
  writeTVar debugMonadoHudPendingGhcGCFrameTimeSamplesVar Seq.empty
  return $ Foldable.toList samples

alignGhcGCSamplesWithOpenXRFrameTimingSamples :: [Double] -> [OpenXRFrameTimingSample] -> [GhcGCFrameTimingSample]
alignGhcGCSamplesWithOpenXRFrameTimingSamples ghcGCFrameTimeSamples openXRFrameTimingSamples =
  zipWith ghcGCFrameTimingSampleForOpenXRFrameTimingSample alignedGcSamples alignedOpenXRFrameTimingSamples
  where
    pairCount = min (length ghcGCFrameTimeSamples) (length openXRFrameTimingSamples)
    alignedGcSamples = drop (length ghcGCFrameTimeSamples - pairCount) ghcGCFrameTimeSamples
    alignedOpenXRFrameTimingSamples = drop (length openXRFrameTimingSamples - pairCount) openXRFrameTimingSamples

ghcGCFrameTimingSampleForOpenXRFrameTimingSample :: Double -> OpenXRFrameTimingSample -> GhcGCFrameTimingSample
ghcGCFrameTimingSampleForOpenXRFrameTimingSample gcMs openXRFrameTimingSample =
  GhcGCFrameTimingSample
    { ghcGCFrameTimingSampleGcMs = gcMs
    , ghcGCFrameTimingSampleNonGCGodotFrameStartToXrWaitFrameMs =
        openXRFrameTimingSampleGodotFrameStartToXrWaitFrameMs openXRFrameTimingSample - gcMs
    }

trimSeqToMax :: Int -> Seq a -> Seq a
trimSeqToMax maxCount values
  | Seq.length values <= maxCount = values
  | otherwise = trimSeqToMax maxCount (Seq.drop 1 values)
