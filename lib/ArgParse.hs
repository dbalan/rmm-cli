{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ArgParse
  ( parseArgs
  ) where

import Options.Applicative

import Types

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
  argConfig <- strOption
              ( long "config-file"
             <> short 'c'
             <> metavar "FILE"
             <> value "/home/dj/.config/rmm/config.ini" )
  argCmd <- cmdParser
  pure Args{..}

opts :: ParserInfo Args
opts = info (argParser <**> helper)
  ( fullDesc
  <> progDesc "CLI Companion for RTM"
  <> header "So that you don't have to remember" )

parseArgs :: IO Args
parseArgs = execParser opts
