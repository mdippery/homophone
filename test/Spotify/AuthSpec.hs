module Spotify.AuthSpec where

import Test.Hspec
import Spotify.Auth

spec :: Spec
spec = do
  let cid   = "abcdefg"
      sec   = "hijklmn"
      creds = Credentials cid sec

  describe "clientIdentifier" $ do
    it "returns the client identifier" $ do
      clientIdentifier creds == cid

  describe "clientSecret" $ do
    it "returns the client secret" $ do
      clientSecret creds == sec

  describe "basicAuthorizationToken" $ do
    it "returns the payload for the Authorization header" $ do
      basicAuthorizationToken creds == "YWJjZGVmZzpoaWprbG1u"
