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
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import Data.Version (showVersion)

import Homophone.Configuration (configurationValue)
import Spotify.Artist (artist, artistName, relatedArtists)
import Spotify.Auth (Credentials(..), authorize)
import qualified Paths_homophone as P

lower :: String -> String
lower = map toLower

oops :: String -> IO ()
oops = hPutStrLn stderr

die :: Int -> String -> IO ()
die code msg = do
  oops msg
  exitWith (ExitFailure code)

artists :: String -> IO ()
artists q = do
  app' <- configurationValue "spotify.client_id"
  secret' <- configurationValue "spotify.client_secret"
  case (app', secret') of
    (Right app, Right secret) -> do
      auth <- authorize (Credentials app secret)
      a <- artist auth q
      other <- relatedArtists auth a
      putStrLn $ intercalate "\n" $ sortBy (comparing lower) $ map artistName other
    (Left s, _) ->
      die 1 s
    (_, Left s) ->
      die 1 s

version :: IO ()
version = do
  let v = showVersion P.version
  p <- getProgName
  putStrLn $ printf "%s v%s" p v

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("-V":_)        -> version
    ("--version":_) -> version
    (artist:_)      -> artists artist
