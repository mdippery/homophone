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


module Main where

import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List (intercalate, sortBy)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

import Data.Version (showVersion)

import Homophone.Configuration (configurationValue)
import Spotify.Artist (artist, name, relatedArtists)
import Spotify.Auth (Credentials(..), authorize)
import qualified Paths_homophone as P

lower :: String -> String
lower = map toLower

artists :: String -> IO String
artists q = do
  app <- configurationValue "spotify.client_id"
  secret <- configurationValue "spotify.client_secret"
  auth <- authorize (Credentials app secret)
  a <- artist auth q
  other <- relatedArtists auth a
  return $ intercalate "\n" $ sortBy (comparing lower) $ map name other

version :: IO String
version = do
  let v = showVersion P.version
  p <- getProgName
  return $ printf "%s v%s" p v

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("-V":_) ->
      version >>= putStrLn
    ("--version":_) ->
      version >>= putStrLn
    (artist:_) ->
      artists artist >>= putStrLn
