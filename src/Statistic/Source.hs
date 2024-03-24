{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where
import Data.Map (Map, fromListWith, toList)
import Data.List (sort)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences = fromListWith (+) . map (\x -> (x, 1))

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy xs = negate . sum $ map prob counts
  where
    n = fromIntegral (length xs)
    counts = map (\(_, c) -> (fromIntegral c / n, c)) . toList . occurrences $ xs
    prob (p, c) = p * log2 (p / (fromIntegral c / n))

-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts = sort . map (\(x, c) -> (x, c)) . toList . occurrences

log2 :: Double -> Double
log2 x = logBase 2 x