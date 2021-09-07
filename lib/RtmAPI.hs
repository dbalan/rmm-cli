{-# LANGUAGE OverloadedStrings #-}

module RtmAPI
  ( buildLoginUrl
  , getFrob
  , getToken
  , getTasks
  , queryTasks
  )
where

import           Control.Monad.IO.Class

import           Control.Monad.Reader
import           Crypto.Hash
import           Data.Aeson (FromJSON)
import           Data.ByteString (ByteString)
import           Data.Sort (sortOn)
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Req

import           Types

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
sign :: Text -> [(Text, Text)] -> Text
sign secret params = T.pack $ show $ hashWith MD5 (TE.encodeUtf8 payload)
  where
    sparams = sortOn fst params
    payload = secret <> T.concat (fmap (uncurry (<>)) sparams)

-- Initial API Ops
getFrob :: Text -> Text -> IO (APIResponse FrobKey)
getFrob secret apiKey = runReq defaultHttpConfig $ do
    let sig = sign secret [("api_key", apiKey), ("format", "json"), ("method", "rtm.auth.getFrob") ]
        opts = ("method" =: ("rtm.auth.getFrob" :: Text) <> "api_key" =: apiKey <> "format" =: ("json" :: Text) <> "api_sig" =: sig ) :: Option https

    r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse opts

    liftIO $ pure (responseBody r)

getToken :: Text -> Text -> FrobKey -> IO (APIResponse Auth)
getToken secret apiKey (FrobKey frob) = runReq defaultHttpConfig $ do
  let params = sortOn fst [("api_key", apiKey), ("format", "json"), ("method", "rtm.auth.getToken"), ("frob", frob)] :: [(Text, Text)]
      sig = sign secret params
      sigparams = params ++ [("api_sig", sig)]
      opts = Prelude.foldl (<>) mempty $ fmap (uncurry (=:)) sigparams :: Option https

  -- liftIO $ putStrLn $ show $ sigparams
  r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse opts
  liftIO $ pure (responseBody r)

-- | Actual API Usage
getTasks :: Text -> Text -> Text -> Text -> IO (ByteString)
getTasks secret apiKey token qry = runReq defaultHttpConfig $ do
  let params = sortOn fst [("api_key", apiKey), ("auth_token", token), ("format", "json"), ("method", "rtm.tasks.getList"), ("filter", qry)] :: [(Text, Text)]
      sig = sign secret params
      sigparams = params ++ [("api_sig", sig)]
      opts = Prelude.foldl (<>) mempty $ fmap (uncurry (=:)) sigparams :: Option https
  r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody bsResponse opts
  liftIO $ pure (responseBody r :: ByteString)

commonParams :: Config -> Method -> [(Text, Text)]
commonParams cfg (Method m) = [("api_key", cfApikey cfg), ("auth_token", cfToken cfg), ("format", "json"), ("method", m)]

makeRequest :: (Show a, FromJSON a) => Method -> [(Text, Text)] -> RtmApiM a
makeRequest m params = do
  cfg <- ask
  let fullParams = sortOn fst (params ++ commonParams cfg m)
      sig = sign (cfSecret cfg) fullParams
      sigparams = fullParams <> [("api_sig", sig)]
      opts = Prelude.foldl (<>) mempty $ fmap (uncurry (=:)) sigparams :: Option https
  runReq defaultHttpConfig $ do
    r <- req GET (https "api.rememberthemilk.com" /: "services" /: "rest") NoReqBody jsonResponse opts
    liftIO $ pure (responseBody r)

queryTasks :: Text -> RtmApiM (APIResponse TaskListResp)
queryTasks q = makeRequest (Method "rtm.tasks.getList") [("filter", q)]

