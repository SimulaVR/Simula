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

import Foreign

import Plugin.SimulaViewSprite
import Plugin.Types

type SurfaceMap = M.Map SimulaView GodotSimulaViewSprite -- C2HS version

data Payload = Payload
               { numWindowsOpen :: TVar SurfaceMap
               , minutesElapsedSinceLastPayload :: Double
               , minutesTotalSession :: Double
               }

mixPanelToken = "d893bae9548d8e678cef251fd81df486" :: String

generateSessionUUID :: IO (UUID)
generateSessionUUID = do exists                 <- doesFileExist "./UUID"
                         maybeRandomUUID             <- Data.UUID.V1.nextUUID
                         let randomUUID = fromMaybe nil maybeRandomUUID
                         let randomUUIDToString =  (Data.UUID.toString randomUUID)
                         -- unless exists          $  appendFile "UUID" randomUUIDToString
                         writeFile "UUID"       randomUUIDToString
                         strUUID                <- readFile "./UUID"
                         let maybeUUID          = Data.UUID.fromString (strUUID)
                         return                 $ case maybeUUID   of
                                                       (Just uuid) -> uuid
                                                       Nothing     -> (nil :: UUID)

ensureUUIDIsRegistered :: UUID -> IO ()
ensureUUIDIsRegistered uuid = do
  let uuidStr = Data.UUID.toString uuid :: String

  -- Get the HTTP connection manager with default TLS settings.
  manager <- newManager tlsManagerSettings
  utcCurrentTime <- getCurrentTime
  let iso8601CurrentTimeStr = formatISO8601 utcCurrentTime
  -- print iso8601CurrentTimeStr

  -- Construct a bytestring encoded request object with the properties we want.
  let encodedRequestObject = LBS.toStrict (encode
        (object
            [ "$token" .= (mixPanelToken :: String)
            , "$distinct_id" .= (uuidStr :: String)
            , "$set_once" .= object [ "firstLoginDate" .= (iso8601CurrentTimeStr :: String)  -- QWERTY: iso-8601 string
                                    , "$name"            .= (uuidStr :: String)
                                    ]
            ]))
  -- 'mappend' the query parameter and base64 encode it.
  let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

  -- Construct the 'Request'
  initialRequest <- parseRequest "http://api.mixpanel.com/engage/"
  let request = initialRequest
        { method = "POST"
        , queryString = requestQueryString
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }

  -- Send the request and print the response.
  response <- httpLbs request manager
  return ()

forkSendAppLaunchEvent :: UUID -> IO ThreadId
forkSendAppLaunchEvent uuid = forkIO $ do
  let uuidStr = Data.UUID.toString uuid :: String

  -- Get the HTTP connection manager with default TLS settings.
  manager <- newManager tlsManagerSettings

  -- Construct a bytestring encoded request object with the properties we want.
  let encodedRequestObject = LBS.toStrict (encode
        (object
            [ "event" .= ("App launched." :: String)
            , "properties"  .=  object [  "distinct_id" .= (uuidStr :: String)
                                       ,  "token"       .= (mixPanelToken :: String)
                                       ]
            ]))
  -- 'mappend' the query parameter and base64 encode it.
  let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

  -- Construct the 'Request'
  initialRequest <- parseRequest "http://api.mixpanel.com/track/"
  let request = initialRequest
        { method = "POST"
        , queryString = requestQueryString
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }

  response <- httpLbs request manager
  return ()

forkSendPayloadEveryMinuteInterval :: UUID -> TVar SurfaceMap -> Integer -> IO ThreadId
forkSendPayloadEveryMinuteInterval uuid tvarSurfaceMap min = forkIO $ (doLoop 0)
    where
    doLoop :: Double -> IO ()
    doLoop dbl = do
        threadDelay (1000 * 1000 * 60 * (fromIntegral min))
        forkSendPayload uuid (Payload { numWindowsOpen = tvarSurfaceMap
                                      , minutesElapsedSinceLastPayload = (fromIntegral min)
                                      , minutesTotalSession = (dbl + (fromIntegral min))
                                      })
        doLoop (dbl + (fromIntegral min))

forkSendPayload :: UUID -> Payload -> IO ()
forkSendPayload uuid (Payload { numWindowsOpen = tvarSurfaceMap
                              , minutesElapsedSinceLastPayload = minutesElapsedSinceLastPayload'
                              , minutesTotalSession = minutesTotalSession'
                              }) = do
  let uuidStr = Data.UUID.toString uuid :: String

  -- Get the HTTP connection manager with default TLS settings.
  manager <- newManager tlsManagerSettings

  --  readMvarInt <- readMVar mvarInt
  surfaceMap  <- readTVarIO tvarSurfaceMap
  let numSurfaces = M.size surfaceMap

  -- Construct a bytestring encoded request object with the properties we want.
  let encodedRequestObject = LBS.toStrict (encode 
        (object
            [ "event" .= ("Payload" :: String)
            , "properties"  .=  object [  "distinct_id"  .= (uuidStr :: String)
                                       ,  "token"        .= (mixPanelToken :: String)
                                       ,  "minutesTotalSession" .= (minutesTotalSession' :: Double)
                                       ,  "minutesElapsedSinceLastPayload" .=    (minutesElapsedSinceLastPayload' :: Double)
                                       ,  "numWindowsOpen" .= (numSurfaces:: Int)
                                       ]
            ]))

  -- 'mappend' the query parameter and base64 encode it.
  let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

  -- Construct the 'Request'
  initialRequest <- parseRequest "http://api.mixpanel.com/track/"
  let request = initialRequest
        { method = "POST"
        , queryString = requestQueryString
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }
  response <- httpLbs request manager
  return ()

-- The first variable encodes the number of windows open (possibly useful data).
startTelemetry :: TVar SurfaceMap -> IO ()
startTelemetry tvarSurfaceMap = do uuid <- generateSessionUUID
                                   forkIO $ ensureUUIDIsRegistered uuid
                                   forkSendPayloadEveryMinuteInterval uuid tvarSurfaceMap 5 -- Send payload every 5 minutes
                                   return ()
