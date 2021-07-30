{-# LANGUAGE OverloadedStrings #-}

module RtmAPISpec
  ( rtmAPISpec
  )
where

import Data.Text
import Test.Hspec

import RtmAPI

rtmAPISpec :: Spec
rtmAPISpec = describe "buildurl is built properly" $ do
  it "example for buildUrl must work" $ do
    buildLoginUrl "abc123" "foo" `shouldBe` "https://www.rememberthemilk.com/services/auth/?api_key=abc123&frob=foo&perms=delete&api_sig=f2ec2349c9e23505098c34fc1bf71a81"

