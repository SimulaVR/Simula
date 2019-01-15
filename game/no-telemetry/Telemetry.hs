{-# LANGUAGE OverloadedStrings   #-}

module Telemetry where

import           Control.Concurrent.STM.TVar

startTelemetry :: TVar a -> IO ()
startTelemetry _tvarSurfaceMap = return ()
