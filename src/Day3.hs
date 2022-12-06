module Day3
  (day3Part1, day3Part2)
where

import Data.Map(Map)
import Data.Set(Set)
import Prelude
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable(find)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)


-- 1. split in 2 compartments
splitInTwo :: String -> (String, String)
splitInTwo s = List.splitAt (length s `div` 2) s

-- 2. all items
setOfItems :: String -> Set Char
setOfItems = Set.fromList

-- 3. duplicated items
duplicatedItems :: Set Char -> Set Char -> Set Char
duplicatedItems = Set.intersection


itemPriorities :: Map Char Integer
itemPriorities = Map.fromList (['a'..'z'] `zip` [1..26] ++ ['A'..'Z'] `zip` [27..52])

-- 4. find priority of character
itemPriority :: Char -> Integer
itemPriority c = itemPriorities Map.! c

toList :: (a, a) -> [a]
toList (l, r) = [l, r]

onlyOne :: Show c => Set c -> c
onlyOne s
  | Set.size s == 1 = fromJust . find (const True) $ s
  | otherwise = error $ "Set can only be of size one" ++ show s

rucksacksCommonItemPriority :: [String] -> Integer
rucksacksCommonItemPriority = itemPriority . onlyOne . foldl1 duplicatedItems . map setOfItems

day3Part1 :: [String] -> Integer
day3Part1 = sum . map (rucksacksCommonItemPriority . toList . splitInTwo)

--- Part 2
--    3 groups
--    EXACTLY one item type must be in all 3 groups.

day3Part2 :: [String] -> Integer
day3Part2 =  sum . map rucksacksCommonItemPriority . chunksOf 3