module Day6 (
firstMarkerPos,
sliding
) where

import qualified Data.Set as Set

-- Laziness at its best
firstMarkerPos :: Int -> String -> Int
firstMarkerPos n line = head [pos | (chunk, pos) <- sliding n line `zip` [n..], allDistinct chunk ]


allDistinct :: Ord a => [a] -> Bool
allDistinct s = length s == (length . Set.fromList) s

sliding :: Int -> [a] -> [[a]]
sliding n xs
  | length xs < n = []
  | otherwise = take n xs : sliding n (drop 1 xs)