module Day1
  (
    maxCaloriesPerElf
  )
where

import Prelude


caloriesPerElf :: Num a => [[a]] -> [a]
caloriesPerElf = map sum


maxCaloriesPerElf :: (Ord a, Num a) => [[a]] -> a
maxCaloriesPerElf = maximum . caloriesPerElf
