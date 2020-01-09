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
-- Module      : Spotify.Service
-- Description : Spotify API client
-- Copyright   : (C) 2019 Michael Dippery
-- License     : LGPL-3
-- Maintainer  : michael@monkey-robot.com
--
-- Implements a client for the Spotify web API.
--
------------------------------------------------------------------------------

module Spotify.Service
  (
    -- * Types
    Application
  , Artist(..)
  , Authorization
  , TokenType(..)

    -- ** Constructors
  , application

    -- * Operations
  , authorize
  , findArtists
  , findBestArtist
  , relatedArtists
  ) where

import Data.Aeson ((.:), FromJSON(..), withObject, withText)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.String.Homophone (lowercase)
import qualified Data.Text as T
import Network.HTTP.Simple
  ( defaultRequest
  , httpJSON
  , getResponseBody
  , setRequestBodyURLEncoded
  , setRequestHeader
  , setRequestHost
  , setRequestMethod
  , setRequestPath
  , setRequestQueryString
  )
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)

-- | Encapsulates the tokens necessary for authenticating and authorization
-- to the Spotify web API.
data Application = Application
  { applicationID     :: String   -- ^ Spotify application (client) ID
  , applicationSecret :: String   -- ^ Spotify application (client) secret
  } deriving Show

-- | Authorization token type.
data TokenType = Bearer | Other String
  deriving Show

instance FromJSON TokenType where
  parseJSON = withText "TokenType" $ \s -> return $
    case lowercase s of
      "bearer"  -> Bearer
      _         -> (Other . T.unpack) s

-- | The result of authorizing with the Spotify web API.
data Authorization = Authorization
  { accessToken :: String     -- ^ Spotify application (client) access token
  , tokenType   :: TokenType  -- ^ Authorization token type
  , expiration  :: Int        -- ^ Authorization expiration timestamp
  } deriving Show

instance FromJSON Authorization where
  parseJSON = withObject "Authorization" $ \v -> Authorization
    <$> v .: "access_token"
    <*> v .: "token_type"
    <*> v .: "expires_in"

-- | An artist.
data Artist = Artist
  { spotifyID         :: String     -- ^ Spotify ID number
  , spotifyURI        :: String     -- ^ Spotify URI
  , artistURL         :: String     -- ^ URL for obtaining artist info from the Spotify API
  , artistName        :: String     -- ^ Artist's name
  , artistGenres      :: [String]   -- ^ Musical genres associated with the artist
  , artistPopularity  :: Int        -- ^ Artist's popularity rating on Spotify
  } deriving Show

instance Eq Artist where
  a == b = (lowercase . artistName) a == (lowercase . artistName) b

instance Ord Artist where
  a <= b = (lowercase . artistName) a <= (lowercase . artistName) b

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \v -> Artist
    <$> v .: "id"
    <*> v .: "uri"
    <*> v .: "href"
    <*> v .: "name"
    <*> v .: "genres"
    <*> v .: "popularity"

newtype SearchResult = SearchResult { artists :: [Artist] }
  deriving Show

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v -> SearchResult
    <$> ((v .: "artists") >>= (.: "items"))

newtype RelatedArtistResult = RelatedArtistResult { results :: [Artist] }
  deriving Show

instance FromJSON RelatedArtistResult where
  parseJSON = withObject "RelatedArtistResult" $ \v -> RelatedArtistResult
    <$> v .: "artists"

-- | Creates a new Spotify application from the given credentials.
application
  :: String       -- ^ Spotify application (client) ID
  -> String       -- ^ Spotify application (client) secret
  -> Application  -- ^ A new application
application = Application

-- | Authorizes the application to make further calls to the Spotify web API.
authorize :: Application -> IO Authorization
authorize app =
  let token   = unpack $ encode $ pack $ applicationID app ++ ":" ++ applicationSecret app
      authHdr = "Basic " ++ token
      req     = setRequestHeader "Authorization" [pack authHdr]
              $ setRequestBodyURLEncoded [("grant_type", "client_credentials")]
                "POST https://accounts.spotify.com/api/token"
   in httpJSON req >>= (return . getResponseBody)

-- | Finds all artists that match the given search query.
findArtists :: Authorization -> String -> IO [Artist]
findArtists Authorization{..} q =
  let req = setRequestHeader "Authorization" [pack ("Bearer " ++ accessToken)]
          $ setRequestQueryString [("type", Just "artist"), ("q", Just (pack q))]
            "GET https://api.spotify.com/v1/search"
   in httpJSON req >>= (return . artists . getResponseBody)

-- | Finds a single artist that best matches the given search query.
findBestArtist :: Authorization -> String -> IO Artist
findBestArtist auth q =
  findArtists auth q >>= (return . head)

-- | Finds all artists that are similar to the given artist.
relatedArtists :: Authorization -> Artist -> IO [Artist]
relatedArtists Authorization{..} Artist{..} =
  let path  = uriPath (fromJust $ parseURI artistURL) ++ "/related-artists"
      host  = uriRegName $ fromJust $ uriAuthority $ fromJust $ parseURI artistURL
      req   = setRequestHeader "Authorization" [pack ("Bearer " ++ accessToken)]
            $ setRequestMethod "GET"
            $ setRequestHost (pack host)
            $ setRequestPath (pack path)
              defaultRequest
   in httpJSON req >>= (return . sort . results . getResponseBody)
