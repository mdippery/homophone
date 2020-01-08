{-
Homophone - Discover related artists
Copyright (C) 2020 Michael Dippery <michael@monkey-robot.com>

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

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


------------------------------------------------------------------------------
-- |
-- Module      : Data.String.Homophone
-- Description : Additional String functions
-- Copyright   : (C) 2020 Michael Dippery
-- License     : LGPL-3
-- Maintainer  : michael@monkey-robot.com
--
-- Extends the base implementation of @Data.String@ with additional functions.
--
------------------------------------------------------------------------------

module Data.String.Homophone
  (
    -- * Types
    StringLike(..)

    -- * Functions
  , lowercase
  ) where

import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T

-- | Denotes types that can be converted into strings or treated like a
-- string.
class StringLike a where
  unpack :: a -> String

instance StringLike String where
  unpack = id

instance StringLike Text where
  unpack = T.unpack

-- | Converts a string to lowercase.
lowercase :: StringLike a => a -> String
lowercase = map toLower . unpack
