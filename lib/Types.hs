{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}


module Types where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
import Data.Time
import Deriving.Aeson
import Utils

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
  { argConfig :: FilePath
  , argCmd :: Cmd }
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

-- API RESPONSE Types ----------------------------------------
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
    id' <- u .: "id"
    username <- u .: "username"
    fullname <- u .: "fullname"
    pure (Auth tkn perms id' username fullname)

newtype TaskListResp = TaskListResp [TaskList] deriving (Show)

instance FromJSON TaskListResp where
  parseJSON = withObject "tsl" $ \o -> do
    r <- o .: "tasks"
    tsl <- r .: "list"
    pure (TaskListResp tsl)

-- Underlying data types --------------------------------------
data Priority = NoPri | Urgent | Important | Normal deriving (Show, Eq)
data Status = Yes | No deriving (Show, Eq)

instance FromJSON Status where
  parseJSON (String "0") = pure No
  parseJSON (String "")  = pure No
  parseJSON (String "1") = pure Yes
  parseJSON _            = fail "unexpected status"


instance ToJSON Status where
  toJSON = String . showT

data TaskList = TaskList
  { tlId :: Text
  , tlTaskseries :: [TaskSeries]
  } deriving Show
    deriving Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "tl", CamelToSnake]] TaskList

data TaskSeries = TaskSeries
            { tsId :: Text
            , tsName :: Text
            , tsCreated :: ZonedTime
            , tsModified :: ZonedTime
            , tsSource :: Text
            , tsUrl :: Text
            , tsLocationId :: Text
            -- TODO: , tsTags :: Maybe Tags
            , tsParticipants :: [Text]
            -- TODO: rrule
            -- TODO: notes
            , tsTask :: [Task]
            } deriving Show
              deriving Generic
              deriving (FromJSON, ToJSON)
              via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "ts", CamelToSnake]] TaskSeries

data Tags = Tags { tgsTag :: [Text]
                 } deriving Show
                   deriving Generic
                   deriving (FromJSON, ToJSON)
                   via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "tgs", CamelToSnake]] Tags

data Task = Task
  { tskId :: Text
  , tskDue :: ZonedTime
  , tskHasDueTime :: Status
  , tskAdded :: ZonedTime
  , tskCompleted :: Status
  , tskDeleted :: Status
  , tskPriority :: Priority
  , tskPostponed :: Status
  , tskEstimate :: Text -- TODO: parse
  } deriving Show
    deriving Generic
    deriving (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "tsk", CamelToSnake]] Task

instance FromJSON Priority where
  parseJSON (String "1") = pure Urgent
  parseJSON (String "2") = pure Important
  parseJSON (String "N") = pure Normal
  parseJSON _            = pure NoPri

instance ToJSON Priority where
  toJSON = String . showT

