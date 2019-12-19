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


------------------------------------------------------------------------------
-- |
-- Module      : Homophone.Configuration
-- Description : Application configuration
-- Copyright   : (C) 2019 Michael Dippery
-- License     : LGPL-3
-- Maintainer  : michael@monkey-robot.com
--
-- Allows the Homophone application to retrieve its configuration options.
--
------------------------------------------------------------------------------

module Homophone.Configuration where

import System.Directory (XdgDirectory(..), getXdgDirectory)
import System.IO (FilePath(..))

import Data.Ini (lookupValue, readIniFile)
import System.FilePath ((</>))
import Data.Text (pack, unpack)

-- | Path to the directory where configuration files are stored.
configurationDirectory :: IO FilePath
configurationDirectory = getXdgDirectory XdgConfig "homophone"

-- | Path to the configuration file.
configurationFile :: IO FilePath
configurationFile = do
  base <- configurationDirectory
  return $ base </> "config.ini"

-- | Returns the value of a configuration option.
--
-- The value should be a period-separated path consisting of the section
-- and key name. For example, to get the Spotify application ID, pass
-- @spotify.client_id@ as the key parameter.
--
-- If an error occurs while retrieving the value, this function will return
-- a @Left@; otherwise, a @Right@ containing the value (as a string) will be
-- returned.
configurationValue :: Read a => String -> IO (Either String a)
configurationValue name = do
  let (section, key) = span (/= '.') name
  path <- configurationFile
  res <- readIniFile path
  case res of
    Left s -> return $ Left s
    Right f ->
      case lookupValue (pack section) (pack (tail key)) f of
        Left s -> return $ Left s
        Right v -> return $ Right $ read $ unpack v
