{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}


{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Types where

import Control.Monad.Reader
import Data.Aeson
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


newtype APIError = APIError Text deriving (Show, Eq)

instance FromJSON APIError where
  parseJSON = withObject "api error" $ \o -> do
    rsp <- o .: "rsp"
    status <- rsp .: "stat"
    case status of
      (String "err")  -> geterr rsp
      (String "fail") -> geterr rsp
      _               -> fail "not an API Error"
    where
      geterr r = do
        err <- r  .: "err"
        msg <- err .: "msg"
        pure (APIError msg)

data APIResponse a = APIResponse {
                       -- | unwrap
                       uneither :: Either APIError a }

deriving instance Show a => Show (APIResponse a)


instance (FromJSON a) => FromJSON (APIResponse a) where
  parseJSON = withObject "api error" $ \o -> do
    rsp <- o .: "rsp"
    status <- rsp .: "stat"
    case status of
      (String "ok") -> do
        r' <- parseJSON (Object rsp)
        pure (APIResponse $ Right r')
      _ -> do
        e <- parseJSON (Object o)
        pure (APIResponse $ Left e)

newtype FrobKey = FrobKey Text deriving (Show, Eq)

instance FromJSON FrobKey where
  parseJSON = withObject "frob response" $ \o -> do
    r <- o .: "frob"
    pure (FrobKey r)


data Auth = Auth
            { authToken :: Text
            , authPerms :: Text
            , authId :: Text
            , authUsername :: Text
            , authFullname :: Text }
          deriving (Show, Eq)

instance FromJSON Auth where
  parseJSON = withObject "auth" $ \o -> do
    r <- o .: "auth"
    tkn <- r .: "token"
    perms <- r .: "perms"
    u <- r .: "user"
    id <- u .: "id"
    username <- u .: "username"
    fullname <- u .: "fullname"
    pure (Auth tkn perms id username fullname)
