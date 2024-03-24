{- |
  Module : LZ.Dictionaries
  Description : A module representing dictionaries from LZ methods
  Maintainer : Romain DUJOL
-}
module LZ.Dictionaries(empty, ascii, zeroAsChar, Dictionary) where

import Data.Char (chr)
import Data.List (singleton)

-- A dictionary is merely an indexed sequence of string
type Dictionary = [String]

-- | The empty dictionary
empty :: Dictionary
empty = [""]

-- | The ASCII dictionary
ascii :: Dictionary
ascii = map (singleton . chr) [0..255]

-- | The zero-valued character
zeroAsChar :: Char
zeroAsChar = Data.Char.chr 0
