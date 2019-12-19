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
{-# LANGUAGE RecordWildCards   #-}


------------------------------------------------------------------------------
-- |
-- Module      : Spotify.Artist
-- Description : Spotify artist data
-- Copyright   : (C) 2019 Michael Dippery
-- License     : LGPL-3
-- Maintainer  : michael@monkey-robot.com
--
-- Retrieves Spotify's data for artists.
--
------------------------------------------------------------------------------

module Spotify.Artist
  (
    -- * Types
    Artist(..)
  , artist

    -- * Spotify API

    -- ** Operations
  , relatedArtists
  , search
  ) where

import qualified Network.HTTP.Simple as H

import Data.Function  (on)
import Data.List      (intercalate, sortBy)
import Data.Maybe     (fromJust)

import Data.Aeson             ((.:), FromJSON(..), withObject)
import Data.ByteString.Char8  (pack)
import Network.HTTP.Client    (Request)
import Network.URI            (parseURI, uriAuthority, uriPath, uriRegName)

import Spotify.Auth (Authorization(..))

-- | An artist.
data Artist = Artist
  { spotifyId :: String   -- ^ Spotify ID number
  , spotifyUri :: String  -- ^ Spotify URI
  , artistUrl :: String   -- ^ URL for obtaining artist info from the Spotify API
  , artistName :: String  -- ^ Artist's name
  , genres :: [String]    -- ^ Musical genres associated with the artist
  , popularity :: Int     -- ^ Artist's popularity rating on Spotify
  } deriving Show

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v -> Artist
    <$> v .: "id"
    <*> v .: "uri"
    <*> v .: "href"
    <*> v .: "name"
    <*> v .: "genres"
    <*> v .: "popularity"

data SearchResult = SearchResult { artists :: [Artist] } deriving Show

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v -> SearchResult
    <$> ((v .: "artists") >>= (.: "items"))

data RelatedArtists = RelatedArtists { related :: [Artist] } deriving Show

instance FromJSON RelatedArtists where
  parseJSON = withObject "RelatedArtists" $ \v -> RelatedArtists
    <$> v .: "artists"

-- | Builds a request for the given artist query using the given
-- authorization token.
searchRequest :: Authorization -> String -> Request
searchRequest Authorization{..} artist
  = H.setRequestHeader "Authorization" [pack ("Bearer " ++ accessToken)]
  $ H.setRequestQueryString [("type", Just "artist"), ("q", Just (pack artist))]
  $ "GET https://api.spotify.com/v1/search"

-- | Builds a request for related artists using the given artist and
-- authorization token.
relatedArtistsRequest :: Authorization -> Artist -> Request
relatedArtistsRequest Authorization{..} artist
  = H.setRequestHeader "Authorization" [pack ("Bearer " ++ accessToken)]
  $ H.setRequestMethod "GET"
  $ H.setRequestHost (pack $ uriRegName $ fromJust $ uriAuthority $ fromJust $ parseURI $ artistUrl artist)
  $ H.setRequestPath (pack $ relatedArtistsUrl artist)
  $ H.defaultRequest

-- | URL for retrieving related artists from the Spotify API.
relatedArtistsUrl :: Artist -> String
relatedArtistsUrl Artist{..} =
  intercalate "/" [(uriPath $ fromJust $ parseURI artistUrl), "related-artists"]

-- | Returns a list of all artists that match the given query.
search :: Authorization -> String -> IO [Artist]
search auth query = do
  let request = searchRequest auth query
  response <- H.httpJSON request
  let results = artists $ H.getResponseBody response
  return results

-- | Retrieves the most popular artist for the given query.
artist :: Authorization -> String -> IO Artist
artist auth query = do
  artists <- search auth query
  return $ head $ reverse $ sortBy (compare `on` popularity) artists

-- | Retrieves artists related to the given artist, as determined by Spotify.
relatedArtists :: Authorization -> Artist -> IO [Artist]
relatedArtists auth artist = do
  let request = relatedArtistsRequest auth artist
  response <- H.httpJSON request
  let results = related $ H.getResponseBody response
  return results
