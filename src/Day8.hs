module Day8(

readHeightMap) where

import GHC.Arr (listArray, Array(..))
import Data.Char (digitToInt)
import Data.Array (bounds)
import Data.Array ((!))


--- rectangular height map of [0, 9]
--- 0 smallest, 9 tallest height
--- Outside edge is visible by definition

type Coord = (Int, Int)
type HeightMap = Array Coord Int

readHeightMap :: [String] -> HeightMap
readHeightMap rows = listArray ((1, 1), (h, w)) . concatMap toDigits $ rows
  where toDigits = map digitToInt
        w = length . head $ rows
        h = length rows

-- tallest :: HeightMap -> Dir -> Int
tallest hm delta c@(1, _) =
  -- is out-of-bound then return value

isVisible :: HeightMap -> Coord -> Bool
isVisible _ (1, _) = True -- First Column
isVisible _ (_, 1) = True -- First Row
isVisible hm coord@(i, j)
  | i == fst (snd (bounds hm)) = True -- Last Column
  | j == snd (snd (bounds hm)) = True -- Last Row
  | otherwise = h > h_up isVisible hm (i-1) j    ||
                isVisible hm  i   (j-1) ||
                isVisible hm  i   (j+1) ||
                isVisible hm (i+1) j
  where h_up = hm ! up coord
        h_down = hm ! down coord
        h_left = hm ! left coord
        h_right = hm ! right coord
        h = hm ! coord

up :: Coord -> Coord
up (i, j) = (i-1, j)

down :: Coord -> Coord
down (i, j) = (i+1, j)

left :: Coord -> Coord
left (i, j) = (i, j-1)

right :: Coord -> Coord
right (i, j) = (i, j+1)





--- OPT ME: Possibly use a mutable array, let be lazy and not too, and hope
--- haskell lazy evaluation does the work for us

-- import qualified Data.Array.IO as IA

-- data Direction = Top | Bot | Left | Right deriving (Show, Eq, Enum, Ord)

-- type VisibleFrom = IA.IOArray Direction Maybe Bool

-- type VisibilityMap = Array (Int, Int) VisibleFrom

-- newVisibleFrom =
-- newVisibilityMap -> (Int, Int) -> MVisbilityMap
