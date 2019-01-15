{-# LANGUAGE OverloadedStrings   #-}

module Telemetry where

import           Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as M

import           Simula.Weston
import           Plugin.Window

type SurfaceMap = M.Map WestonSurface Window

startTelemetry :: TVar SurfaceMap -> IO ()
startTelemetry _tvarSurfaceMap = return ()
