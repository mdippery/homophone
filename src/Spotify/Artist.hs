{-
Homophone - Discover related artists
Copyright (C) 2019 Michael Dippery <michael@monkey-robot.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}


module Spotify.Artist
  (
    -- * Types
    Artist(..)

    -- * Helpers
  , searchRequest
  ) where

import Data.Aeson             ((.:), FromJSON(..), withObject)
import Data.ByteString.Char8  (pack)
import Network.HTTP.Client    (Request)
import Network.HTTP.Simple    (setRequestHeader, setRequestQueryString)

import Spotify.Auth (Credentials, basicAuthorizationToken)

data Artist = Artist
  { spotifyId :: String
  , spotifyUri :: String
  , name :: String
  , genres :: [String]
  , popularity :: Int
  } deriving Show

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v -> Artist
    <$> v .: "id"
    <*> v .: "uri"
    <*> v .: "name"
    <*> v .: "genres"
    <*> v .: "popularity"

searchRequest :: Credentials -> String -> Request
searchRequest creds artist
  = setRequestHeader "Authorization" [pack ("Basic " ++ basicAuthorizationToken creds)]
  $ setRequestQueryString [("type", Just "artist"), ("q", Just (pack artist))]
  $ "GET https://api.spotify.com/v1/search"
