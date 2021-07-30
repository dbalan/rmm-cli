{-# LANGUAGE OverloadedStrings #-}
module RtmAPI
  ( buildLoginUrl
  , getFrob
  , getToken
  , getTasks
  , Response(..)
  )
where

import           Control.Lens
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString (ByteString, fromStrict)
import           Data.Sort (sortOn)
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Req

-- | Build login URL from an API key, this following the process descrbed in
-- https://www.rememberthemilk.com/services/api/authentication.rtm
buildLoginUrl :: Text -> Text -> Text -> Text
buildLoginUrl secret apiKey frob = "https://www.rememberthemilk.com/services/auth/?" <> p'
  where
    p' = T.intercalate "&" p
    params :: [(Text, Text)]
    params = [ ("api_key", apiKey)
             , ("frob", frob)
             , ("perms", "delete")
             ]
    sig = ("api_sig", sign secret params)
    p = fmap (\(k,v) -> k <> "=" <> v) (params <> [sig])

-- | sign API params
-- TODO we pull cryptonite anyway
-- TODO: 1. sort api params
-- add shared secret
-- md5
sign :: Text -> [(Text, Text)] -> Text
sign secret params = T.pack $ show $ hashWith MD5 (TE.encodeUtf8 payload)
  where
    sparams = sortOn fst params
    payload = secret <> T.concat (fmap (uncurry (<>)) params)

data Response = Frob Text
              | Err Text
              | Auth { token :: Text
                     , perms :: Text
                     , id :: Text
                     , username :: Text
                     , fullname :: Text }
              deriving (Show, Eq)
instance FromJSON Response where
  parseJSON = withObject "API Response" $ \o -> do
    rsp <- o .: "rsp"
    status <- rsp .: "stat"
    case status of
      (String "err") -> do
        msg <- rsp .: "err" >>= (.: "msg")
        pure (Err msg)
      (String "fail") -> do
        msg <- rsp .: "err" >>= (.: "msg")
        pure (Err msg)
      (String "ok") -> do
        f <- rsp .:? "frob"
        case f of
          Just f  -> pure (Frob f)
          Nothing -> do
            t <- rsp .:? "auth"
            case t of
              Just t' -> do
                token <- t' .: "token"
                perms <- t' .: "perms"
                u <- t' .: "user"
                id <- u .: "id"
                username <- u .: "username"
                fullname <- u .: "fullname"
                pure (Auth token perms id username fullname)
              Nothing -> fail "parse error"


-- API Ops
getFrob :: Text -> Text -> IO (Maybe Response)
getFrob secret apiKey = runReq defaultHttpConfig $ do
    let sig = sign secret [("api_key", apiKey), ("format", "json"), ("method", "rtm.auth.getFrob") ]
        opts = ("method" =: ("rtm.auth.getFrob" :: Text) <> "api_key" =: apiKey <> "format" =: ("json" :: Text) <> "api_sig" =: sig ) :: Option https

    r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse opts

    liftIO $ pure (responseBody r)

getToken :: Text -> Text -> Text -> IO (Maybe Response)
getToken secret apiKey frob = runReq defaultHttpConfig $ do
  let params = sortOn fst [("api_key", apiKey), ("format", "json"), ("method", "rtm.auth.getToken"), ("frob", frob)] :: [(Text, Text)]
      sig = sign secret params
      sigparams = params ++ [("api_sig", sig)]
      opts = Prelude.foldl (<>) mempty $ fmap (uncurry (=:)) sigparams :: Option https

  -- liftIO $ putStrLn $ show $ sigparams
  r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse opts
  liftIO $ pure (responseBody r)

getTasks :: Text -> Text -> Text -> Text -> IO (ByteString)
getTasks secret apiKey token filter = runReq defaultHttpConfig $ do
  let params = sortOn fst [("api_key", apiKey), ("auth_token", token), ("format", "json"), ("method", "rtm.tasks.getList"), ("filter", filter)] :: [(Text, Text)]
      sig = sign secret params
      sigparams = params ++ [("api_sig", sig)]
      opts = Prelude.foldl (<>) mempty $ fmap (uncurry (=:)) sigparams :: Option https
  r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody bsResponse opts
  liftIO $ pure (responseBody r :: ByteString)

