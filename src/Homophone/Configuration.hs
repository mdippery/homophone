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


module Homophone.Configuration where

import System.Directory (XdgDirectory(..), getXdgDirectory)
import System.IO (FilePath(..))

import Data.Ini (lookupValue, readIniFile)
import System.FilePath ((</>))
import Data.Text (pack, unpack)

configurationDirectory :: IO FilePath
configurationDirectory = getXdgDirectory XdgConfig "homophone"

configurationFile :: IO FilePath
configurationFile = do
  base <- configurationDirectory
  return $ base </> "config.ini"

configurationValue :: String -> IO String
configurationValue name = do
  let (section, key) = span (/= '.') name
  path <- configurationFile
  res <- readIniFile path
  case res of
    Left s -> return ""
    Right f ->
      case lookupValue (pack section) (pack (tail key)) f of
        Left s -> return ""
        Right v -> return (unpack v)
