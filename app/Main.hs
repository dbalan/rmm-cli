{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Byline
import           Config.Schema
import           Control.Applicative
import           Control.Exception (throw, try)
import           Control.Monad.IO.Class
import           Data.Text as T
import qualified Data.Text.IO as TIO
import           Options.Applicative
import           RtmAPI
import           System.FilePath


data Cmd =
    LsAll
  | Configure
  | Query Text
  deriving (Show, Eq)

data Args = Args
  { config :: FilePath
  , cmd :: Cmd }
  deriving (Show, Eq)

data Config = Config
  { cfApikey :: Text
  , cfSecret :: Text
  , cfToken :: Text }
  deriving (Show, Eq)

configSpec :: ValueSpec Config
configSpec = sectionsSpec "" $ do
  cfApikey <- reqSection "apikey" "API key of the Application"
  cfSecret <- reqSection "secret" "API shared secret"
  cfToken <- reqSection "token" "Application token"
  pure Config{..}

cmdParser :: Parser Cmd
cmdParser = subparser
  (  command "configure" (info (pure Configure) (progDesc "configure application access (mostly just for first time use)"))
  <> command "list" (info (pure LsAll) (progDesc "list all tasks"))
  <> command "query" (info qry (progDesc "query for specific tasks"))
  )

qry :: Parser Cmd
qry = Query <$> argument str (metavar "QUERY")

-- Applicative do is magic
argParser :: Parser Args
argParser = do
  config <- strOption
              ( long "config-file"
             <> short 'c'
             <> metavar "FILE"
             <> value "/home/dj/.config/rmm/config.ini" )
  cmd <- cmdParser
  pure Args{..}

opts :: ParserInfo Args
opts = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "CLI Companion for RTM"
  <> header "So that you don't have to remember" )

readConfig :: FilePath -> IO Config
readConfig path = do
  cfg <- try $ loadValueFromFile configSpec path
  case (cfg :: Either IOError Config) of
    Left e -> do
      -- no config file, ask user to run configurn
      runBylineT $ sayLn (("Configuration file not found at: " <> text (T.pack . show $ path) <> fg red)
                        <> "\nPlease run configure")
      throw e
    Right cfg' -> pure cfg'

writeConfig :: FilePath -> Text -> Text -> Text -> IO ()
writeConfig path apikey secret token = do
  let c' =
        "apikey: \"" <> apikey <> "\"\n" <>
        "secret: \"" <> secret <> "\"\n" <>
        "token:  \"" <> token <> "\"\n"
  TIO.writeFile path c'

main :: IO ()
main = do
  args <- execParser opts
  -- read config
  let path = config args
  case (cmd args) of
    Configure -> configure path
    LsAll     -> readConfig path >>= (flip list) "dueBefore:tomorrow NOT status:complete"
    Query q   -> readConfig path >>= (flip list) q


configure :: FilePath -> IO ()
configure path = do
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


list :: Config -> Text -> IO ()
list cfg filter = do
  ts <- getTasks (cfSecret cfg) (cfApikey cfg) (cfToken cfg) filter
  putStrLn $ show $ ts

