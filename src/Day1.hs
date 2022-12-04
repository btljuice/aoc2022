module Day1
  (
    maxCalories
  , readCalories
  , caloriesPerElf
  , topCalories
) where

import Data.List.Split ( splitOn )
import Prelude
import Data.List ( sortOn )


readCalories :: [String] -> [[Integer]]
readCalories = map (map read) . splitOn [""]


caloriesPerElf :: Num a => [[a]] -> [a]
caloriesPerElf = map sum


maxCalories :: Ord a => [a] -> a
maxCalories = maximum

topCalories :: Int -> [Integer] -> [Integer]
topCalories x = take x . sortOn negate
