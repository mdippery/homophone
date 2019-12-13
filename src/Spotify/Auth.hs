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

{-# LANGUAGE RecordWildCards #-}


------------------------------------------------------------------------------
-- |
-- Module      : Spotify.Auth
-- Description : Authentication for the Spotify API
-- Copyright   : (C) 2019 Michael Dippery
-- License     : LGPL-3
-- Maintainer  : michael@monkey-robot.com
--
-- Authentication for the Spotify API.
--
------------------------------------------------------------------------------

module Spotify.Auth where

import Data.List (intercalate)

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, unpack)

-- | Credentials needed for access to parts of the Spotify API that do not
-- require authorization.
data Credentials = Credentials
  { clientIdentifier :: String  -- ^ Client application identifier
  , clientSecret :: String      -- ^ Client secret key
  }

instance Show Credentials where
  show Credentials{..} = intercalate ":" [clientIdentifier, clientSecret]

-- | Authorization header payload for the given Spotify credentials.
basicAuthorizationToken :: Credentials -> String
basicAuthorizationToken = unpack . encode . pack . show
