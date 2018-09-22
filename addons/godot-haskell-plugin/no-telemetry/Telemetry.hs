{-# LANGUAGE OverloadedStrings   #-}

module Telemetry where

import           Data.Aeson
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as BSB64
import qualified Data.ByteString.Lazy    as LBS
import           Data.Semigroup          ((<>))
import           Network.HTTP.Client     (getUri, httpLbs, method, newManager,
                                          parseRequest, queryString,
                                          requestHeaders, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types      (statusCode)
import           Control.Concurrent
import           System.Directory
import           Data.UUID
import           Data.UUID.V4
import           Data.UUID.V1
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.ISO8601
import           Control.Concurrent.STM.TVar
import           Data.Maybe
import qualified Data.Map.Strict as M

import Simula.Weston
import Plugin.WestonSurfaceSprite

type SurfaceMap = M.Map WestonSurface GodotWestonSurfaceSprite

startTelemetry :: TVar SurfaceMap -> IO ()
startTelemetry tvarSurfaceMap = return ()
