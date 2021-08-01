
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ConfigParser
  ( loadConfig
  , writeConfig
  ) where

import Config.Schema
import Control.Exception (try)
import Data.Text
import Data.Text.IO as TIO

import Types

configSpec :: ValueSpec Config
configSpec = sectionsSpec "" $ do
  cfApikey <- reqSection "apikey" "API key of the Application"
  cfSecret <- reqSection "secret" "API shared secret"
  cfToken <- reqSection "token" "Application token"
  pure Config{..}

-- | trys to load config, fails if file not found
loadConfig :: FilePath -> IO (Either IOError Config)
loadConfig p = try (loadValueFromFile configSpec p)

writeConfig :: FilePath -> Text -> Text -> Text -> IO ()
writeConfig path apikey secret token = do
  let c' =
        "apikey: \"" <> apikey <> "\"\n" <>
        "secret: \"" <> secret <> "\"\n" <>
        "token:  \"" <> token <> "\"\n"
  TIO.writeFile path c'

