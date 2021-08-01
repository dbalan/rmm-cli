{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Data.Text

data Config = Config
  { cfApikey :: Text
  , cfSecret :: Text
  , cfToken :: Text }
  deriving (Show, Eq)

newtype RtmApiM a = RtmApiM
  { runRtmApiM ::  ReaderT Config IO a
  } deriving (Functor, Applicative, Monad)
    deriving (MonadIO, MonadReader Config)

newtype Method = Method Text deriving (Show, Eq, Ord)

data Cmd =
    LsAll
  | Configure
  | Query Text
  deriving (Show, Eq)

data Args = Args
  { config :: FilePath
  , cmd :: Cmd }
  deriving (Show, Eq)



