{-# LANGUAGE RecordWildCards #-}
module Simula.Telemetry
  ( Telemetry(..)
  , startTelemetry
  , setTelemetryConsent
  , getTelemetryConsent
  , forkSendAppLaunchEvent
  )
where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Base64        as BSB64
import qualified Data.ByteString.Lazy          as LBS
import           Data.Semigroup                           ( (<>) )
import           Data.Time.Clock
import           Data.Time.ISO8601
import           Data.UUID
import           Network.HTTP.Client                      ( httpLbs
                                                          , method
                                                          , newManager
                                                          , parseRequest
                                                          , queryString
                                                          , requestHeaders
                                                          )
import           Network.HTTP.Client.TLS                  ( tlsManagerSettings )
import           Simula.Config


data Telemetry = Telemetry
  { pollNumWindowsOpen :: IO Int
  }

data Payload = Payload
  { numWindowsOpen                 :: Int
  , minutesElapsedSinceLastPayload :: Double
  , minutesTotalSession            :: Double
  }

-- Denotes our MixPanel "project ID".
mixPanelToken :: String
mixPanelToken = "d893bae9548d8e678cef251fd81df486" :: String -- actual Simula token
--            = "5ad417357e3a80fa426d272473d8dee4" :: String -- testing token

setTelemetryConsent :: Bool -> IO ()
setTelemetryConsent consent = readConfig >>= \case
  Just cfg -> writeConfig (cfg { telemetry = consent })
  Nothing  -> do
    cfg <- defaultConfig
    writeConfig $ cfg { telemetry = consent }

getExistingOrNewUUID :: IO UUID
getExistingOrNewUUID = getUUID >>= \case
  Just uuid -> return uuid
  Nothing   -> newUUID >> getUUID >>= \case
    Just uuid -> return uuid
    Nothing   -> error "Failed to parse or generate UUID"

getExistingOrNewUUIDString :: IO String
getExistingOrNewUUIDString = Data.UUID.toString <$> getExistingOrNewUUID


-- TODO: Make this return IO (ExitCode)
ensureUUIDIsRegistered :: IO ()
ensureUUIDIsRegistered = do
  uuidStr        <- getExistingOrNewUUIDString

  -- Get the HTTP connection manager with default TLS settings.
  manager        <- newManager tlsManagerSettings
  utcCurrentTime <- getCurrentTime
  let iso8601CurrentTimeStr = formatISO8601 utcCurrentTime
  -- print iso8601CurrentTimeStr

  -- Construct a bytestring encoded request object with the properties we want.
  let encodedRequestObject = LBS.toStrict
        (encode
          (object
            [ "$token" .= (mixPanelToken :: String)
            , "$distinct_id" .= (uuidStr :: String)
            , "$set_once" .= object
              [ "firstLoginDate" .= (iso8601CurrentTimeStr :: String)  -- QWERTY: iso-8601 string
              , "$name" .= (uuidStr :: String)
              ]
            ]
          )
        )
  -- 'mappend' the query parameter and base64 encode it.
  let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

  -- Construct the 'Request'
  initialRequest <- parseRequest "http://api.mixpanel.com/engage/"
  let request = initialRequest
        { method         = "POST"
        , queryString    = requestQueryString
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }

  -- Send the request and print the response.
  _response <- httpLbs request manager
  return ()

getTelemetryConsent :: IO (Maybe Bool)
getTelemetryConsent = fmap (fmap telemetry) readConfig

forkSendAppLaunchEvent :: IO ThreadId
forkSendAppLaunchEvent = forkIO $ do
  uuidStr <- getExistingOrNewUUIDString

  -- Get the HTTP connection manager with default TLS settings.
  manager <- newManager tlsManagerSettings

  -- Construct a bytestring encoded request object with the properties we want.
  let encodedRequestObject = LBS.toStrict
        (encode
          (object
            [ "event" .= ("App launched." :: String)
            , "properties" .= object
              [ "distinct_id" .= (uuidStr :: String)
              , "token" .= (mixPanelToken :: String)
              ]
            ]
          )
        )
  -- 'mappend' the query parameter and base64 encode it.
  let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

  -- Construct the 'Request'
  initialRequest <- parseRequest "http://api.mixpanel.com/track/"
  let request = initialRequest
        { method         = "POST"
        , queryString    = requestQueryString
        , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
        }

  _response <- httpLbs request manager
  return ()

forkSendPayloadEveryMinuteInterval :: IO Int -> Integer -> IO ThreadId
forkSendPayloadEveryMinuteInterval getNumWindows minutes = forkIO $ (doLoop 0)
 where
  doLoop :: Double -> IO ()
  doLoop dbl = do
    threadDelay (1000 * 1000 * 60 * (fromIntegral minutes))
    numWindows <- getNumWindows
    forkSendPayload
      (Payload
        { numWindowsOpen                 = numWindows
        , minutesElapsedSinceLastPayload = (fromIntegral minutes)
        , minutesTotalSession            = (dbl + (fromIntegral minutes))
        }
      )
    doLoop (dbl + (fromIntegral minutes))

forkSendPayload :: Payload -> IO ()
forkSendPayload (Payload { numWindowsOpen, minutesElapsedSinceLastPayload, minutesTotalSession })
  = do
    uuidStr     <- getExistingOrNewUUIDString

    -- Get the HTTP connection manager with default TLS settings.
    manager     <- newManager tlsManagerSettings

    -- Construct a bytestring encoded request object with the properties we want.
    let
      encodedRequestObject = LBS.toStrict
        (encode
          (object
            [ "event" .= ("Payload" :: String)
            , "properties" .= object
              [ "distinct_id" .= (uuidStr :: String)
              , "token" .= (mixPanelToken :: String)
              , "minutesTotalSession" .= (minutesTotalSession :: Double)
              , "minutesElapsedSinceLastPayload"
                .= (minutesElapsedSinceLastPayload :: Double)
              , "numWindowsOpen" .= (numWindowsOpen :: Int)
              ]
            ]
          )
        )

    -- 'mappend' the query parameter and base64 encode it.
    let requestQueryString = ("data=" <> BSB64.encode encodedRequestObject)

    -- Construct the 'Request'
    initialRequest <- parseRequest "http://api.mixpanel.com/track/"
    let request = initialRequest
          { method         = "POST"
          , queryString    = requestQueryString
          , requestHeaders = [ ( "Content-Type"
                               , "application/json; charset=utf-8"
                               )
                             ]
          }
    _response <- httpLbs request manager
    return ()

-- The first variable encodes the number of windows open (possibly useful data).
startTelemetry :: Telemetry -> IO ()
startTelemetry (Telemetry {..}) = do
  forkIO ensureUUIDIsRegistered
  forkSendPayloadEveryMinuteInterval pollNumWindowsOpen 5 -- Send payload every 5 minutes
  return ()

