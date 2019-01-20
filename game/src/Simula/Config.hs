{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric     #-}
module Simula.Config where

import           Simula.Imports

import           Data.Maybe                               ( fromMaybe )
import qualified Data.Text.IO
import           Data.UUID                                ( UUID
                                                          , fromText
                                                          , nil
                                                          , toText
                                                          )
import           Data.UUID.V1                             ( nextUUID )
import           Dhall
import           Dhall.Core                               ( pretty )
import           Dhall.Format                             ( format )
import           Dhall.Pretty                             ( CharacterSet(..) )
import           System.Directory                         ( doesFileExist )


data Config = Config
  { uuid      :: Text
  , telemetry :: Bool
  , autostart :: [FilePath]
  } deriving (Generic, Show)

instance Interpret Config


-- Relative to the project.godot or Simula binary
cfgPath :: FilePath
cfgPath = "./config"


-- | Generates a new config
defaultConfig :: IO Config
defaultConfig = do
  randomUUID <- fromMaybe nil <$> nextUUID
  return $ Config
    { uuid = toText randomUUID
    , telemetry = False
    , autostart = []
    }

-- | Creates a new default config file
generateConfigFile :: IO ()
generateConfigFile = defaultConfig >>= writeConfig

readConfig :: IO (Maybe Config)
readConfig = do
  exists <- doesFileExist cfgPath
  if exists then Just <$> input auto (pack cfgPath) else return Nothing

writeConfig :: Config -> IO ()
writeConfig cfg = do
  Data.Text.IO.writeFile cfgPath $ toDhallSrc cfg
  format Unicode (Just cfgPath)

-- | Get user's UUID (non-nil, valid UUID only)
getUUID :: IO (Maybe UUID)
getUUID = readConfig <&> \case
  Nothing -> Nothing
  Just (Config { uuid }) ->
    let mayUUID = fromText uuid
    in  if mayUUID == Just nil then Nothing else mayUUID

-- | Create a new UUID (WARNING: Writes to config file)
newUUID :: IO ()
newUUID = readConfig >>= \case
  Nothing      -> generateConfigFile
  Just cfgFile -> do
    randomUUID <- fromMaybe nil <$> nextUUID
    writeConfig $ cfgFile { uuid = toText randomUUID }

-- | Convert Config into valid Dhall source code
toDhallSrc :: Config -> Text
toDhallSrc = pretty . embed injectConfig
 where
  injectConfig :: InputType Config
  injectConfig = inputRecord $ adapt
    >$< inputField "uuid"
    >*< inputField "telemetry"
    >*< inputField "autostart"

  adapt (Config {..}) =
    ( uuid
    , ( telemetry
      , pack <$> autostart
      )
    )
