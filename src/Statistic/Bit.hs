{- |
  Module : Statistic.Bit
  Description : A module representing a bit
  Maintainer : Romain DUJOL
-}
module Statistic.Bit(Bit(Zero, One)) where

-- | A bit is either a zero-valued or one-valued
data Bit = Zero | One
  deriving (Bounded, Enum, Eq, Ord)

instance Show Bit where
  show Zero = "0"
  show One  = "1"

