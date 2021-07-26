{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Text
import Options.Applicative

data Cmd =
    LsAll
  | Query Text
  deriving (Show, Eq, Ord)

argParser :: Parser Cmd
argParser = lsAll <|> qry

lsAll :: Parser Cmd
lsAll = flag' LsAll (long "list" <> help "list all tasks" )

qry :: Parser Cmd
qry = Query <$> strOption
  ( long "query"
  <> short 'q'
  <> metavar "TEXT"
  <> help "Query Tasks" )

opts :: ParserInfo Cmd
opts = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "CLI Companion for RTM"
  <> header "So that you don't have to remember" )

main :: IO ()
main = do
  args <- execParser opts
  putStrLn $ show args

