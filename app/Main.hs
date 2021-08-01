{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Byline

import Control.Applicative
import Control.Exception (throw, try)
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
      runBylineT $ sayLn (("Configuration file not found at: " <> text (T.pack . show $ path) <> fg red)
                        <> "\nPlease run configure")
      throw e
    Right cfg' -> pure cfg'

main :: IO ()
main = do
  args <- parseArgs
  -- read config
  let path = config args
  case (cmd args) of
    Configure -> configure path
    LsAll     -> readConfig path >>= (flip list) "dueBefore:tomorrow NOT status:complete"
    Query q   -> do
      cfg <- readConfig path
      t <- runReaderT (runRtmApiM $ queryTasks q) cfg
      putStrLn $ show t


configure :: FilePath -> IO ()
configure path = do
  -- hard coded application keys, its fine. :|
  let secret = "10fa10b9a5f290fe" :: Text
      apiKey = "76cd370db1c53c387a1aaec1058ffaba" :: Text
  frob <- getFrob secret apiKey
  case frob of
      Nothing -> do
        runBylineT $ sayLn ("Failed generating login url" <> fg red)
        pure ()
      Just (Frob f') -> do
        let loginurl = buildLoginUrl secret apiKey f'
        runBylineT $ do
          sayLn (("Please visit following URL to login: " <> fg green) <> (text loginurl <> fg red))
          askLn ("Press enter when done!\n" <> fg blue) Nothing
          pure ()
        resp <- getToken secret apiKey f'
        case resp of
          Nothing -> do
            runBylineT $ sayLn ("Failed generating login url" <> fg red)
            pure ()
          Just (Auth t p id u fn) -> writeConfig path apiKey secret t
          Just _ -> do
            runBylineT $ sayLn ("Something terrible" <> fg red)
            pure ()


