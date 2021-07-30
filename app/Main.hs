{-# LANGUAGE OverloadedStrings #-}
module Main where

import Byline
import Control.Applicative
import Control.Monad.IO.Class
import Data.Text as T
import Options.Applicative
import RtmAPI

data Cmd =
    LsAll
  | Configure
  | Query Text
  deriving (Show, Eq, Ord)

argParser :: Parser Cmd
argParser = subparser
  (  command "configure" (info (pure Configure) (progDesc "configure application access (mostly just for first time use)"))
  <> command "list" (info (pure LsAll) (progDesc "list all tasks"))
  <> command "query" (info qry (progDesc "query for specific tasks"))
  )

qry :: Parser Cmd
qry = Query <$> argument str (metavar "QUERY")

opts :: ParserInfo Cmd
opts = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "CLI Companion for RTM"
  <> header "So that you don't have to remember" )

main :: IO ()
main = do
  args <- execParser opts
  -- configure
  list

configure :: IO ()
configure = do
  let secret = "10fa10b9a5f290fe"
      apiKey = "76cd370db1c53c387a1aaec1058ffaba"
  frob <- getFrob secret apiKey
  case frob of
      Nothing -> do
        runBylineT $ sayLn ("Failed generating login url" <> fg red)
        pure ()
      Just (Frob f') -> do
        let loginurl = buildLoginUrl secret apiKey f'
        runBylineT $ do
          sayLn (("Please visit following URL to login: " <> fg green) <> (text loginurl <> fg red))
          askLn ("Press enter when done!" <> fg blue) Nothing
          pure ()

        resp <- getToken secret apiKey f'
        putStrLn $ show resp
  pure ()

list :: IO ()
list = do
  let secret = "10fa10b9a5f290fe"
      apiKey = "76cd370db1c53c387a1aaec1058ffaba"
      token = "b8a07b913f0bdb15f7e09b3c3f9fa1c4454d1cf7"
      filter = "dueBefore:tomorrow NOT status:complete" :: Text
  ts <- getTasks secret apiKey token filter
  putStrLn $ show $ ts

