{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Byline

import Control.Exception (throw)
import Control.Monad.Reader
import Data.Text as T

import ArgParse
import ConfigParser
import RtmAPI
import Types

readConfig :: FilePath -> IO Config
readConfig path = do
  cfg <- loadConfig path
  case (cfg :: Either IOError Config) of
    Left e -> do
      -- no config file, ask user to run configurn
      runBylineT $ do
        sayLn (("Configuration file not found at: " <> text (T.pack . show $ path) <> fg red)
                <> "\nPlease run configure")
      throw e
    Right cfg' -> pure cfg'

main :: IO ()
main = do
  args <- parseArgs
  -- read config
  let path = config args
  case cmd args of
    Configure -> configure path
    LsAll     -> do
      cfg <- readConfig path
      t <- runReaderT (runRtmApiM $ queryTasks "dueBefore:tomorrow NOT status:complete") cfg
      print t
    Query q   -> do
      cfg <- readConfig path
      t <- runReaderT (runRtmApiM $ queryTasks q) cfg
      print t


-- TODO: wrap in eitherT
configure :: FilePath -> IO ()
configure path = do
  -- hard coded application keys, its fine. :|
  let secret = "10fa10b9a5f290fe" :: Text
      apiKey = "76cd370db1c53c387a1aaec1058ffaba" :: Text

  frob <- getFrob secret apiKey
  case uneither frob of
    Left (APIError err) -> do
      runBylineT $ sayLn ("Failed generating login url:" <> text err <> fg red)
      pure ()
    Right f@(FrobKey f') -> do
      let loginurl = buildLoginUrl secret apiKey f'
      runBylineT $ do
        sayLn (("Please visit following URL to login: " <> fg green) <> (text loginurl <> fg red))
        askLn ("Press enter when done!\n" <> fg blue) Nothing
      resp <- getToken secret apiKey f
      case uneither resp of
        Left (APIError err) -> do
          runBylineT $ sayLn ("Failed getting authToken:" <> text err <> fg red)
          pure ()
        Right (Auth t _ _ _ _) -> writeConfig path apiKey secret t

