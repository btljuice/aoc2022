module Day8(
  readHeightMap,
  countVisible,
  isVisible,
  isInbound,
  tallest,
  visibleFrom,
  day8part1,
  Dir(..),
  viewDistances,
  viewDistanceFrom,
  day8part2,
) where

import GHC.Arr (listArray, Array(..))
import Data.Char (digitToInt)
import Data.Array ( bounds, (!), indices )
import Prelude hiding (Left, Right)
import qualified Data.Foldable


--- rectangular height map of [0, 9]
--- 0 smallest, 9 tallest height
--- Outside edge is visible by definition

type Coord = (Int, Int)
type HeightMap = Array Coord Int
type Iterator a = [a]
data Dir = Up | Down | Left | Right deriving (Enum, Show, Eq)

readHeightMap :: [String] -> HeightMap
readHeightMap rows = listArray ((1, 1), (nbRows, nbCols)) . concatMap toDigits $ rows
  where toDigits = map digitToInt
        nbCols = length . head $ rows
        nbRows = length rows

day8part1 = countVisible. readHeightMap

countVisible :: HeightMap -> Int
countVisible hm = sum [ fromEnum (isVisible hm c) | c <- indices hm ]

-- ANSWER ME : How lazy can we be ?
-- What is the lifetime / scope that Haskell retains in memory a previously
-- evaluated expression ?
-- otherwise we'll memoize the tallest heights

------ Part 1

tallest :: HeightMap -> Iterator Coord -> Int
tallest hm ite
  | isInbound hm coord = max height (tallest hm (tail ite))
  | otherwise = minBound :: Int
  where coord  = head ite
        height = hm ! coord

isVisible :: HeightMap -> Coord -> Bool
isVisible hm c = not . null $ visibleFrom hm c

visibleFrom :: HeightMap -> Coord -> [Dir]
visibleFrom hm coord =
  flatten [ toMaybe (height > tallest hm (up coord)) Up,
            toMaybe (height > tallest hm (left coord)) Left,
            toMaybe (height > tallest hm (right coord)) Right,
            toMaybe (height > tallest hm (down coord)) Down ]
  where height = hm ! coord


---- Part 2

viewDistanceFrom :: HeightMap -> Int -> Iterator Coord -> Int
viewDistanceFrom hm homeHeight ite
  | not inbound = 0 -- on the edge, there's nothing to see
  | homeHeight <= height = 1
  | otherwise = 1 + viewDistanceFrom hm homeHeight (tail ite) -- Can see next tree and maybe more
  where coord  = head ite
        inbound = isInbound hm coord
        height = hm ! coord

viewDistances :: HeightMap -> Coord -> [Int]
viewDistances hm coord =
  [ viewDistanceFrom hm height (up coord),
    viewDistanceFrom hm height (left coord),
    viewDistanceFrom hm height (right coord),
    viewDistanceFrom hm height (down coord) ]
  where height = hm ! coord

day8part2 :: [String] -> Int
day8part2 = maxViewDistanceScore . readHeightMap

maxViewDistanceScore :: HeightMap -> Int
maxViewDistanceScore hm = maximum [product (viewDistances hm c) | c <- indices hm]



-- Member wise tuple numerical operation.
-- This definition must already exist somewhere
instance (Num a, Num b) => Num (a, b) where
  (+) (i0, j0) (i1, j1) = (i0 + i1, j0 + j1)
  (*) (i0, j0) (i1, j1) = (i0 * i1, j0 * j1)
  abs (i, j) = (abs i, abs j)
  signum (i, j) = (signum i, signum j)
  fromInteger i = (fromInteger i, fromInteger i)
  negate (i, j) = (negate i, negate j)

up :: Coord -> Iterator Coord
up = tail . myIterate (+ (-1, 0))

down :: Coord -> Iterator Coord
down = tail . myIterate (+ (1, 0))

left :: Coord -> Iterator Coord
left = tail . myIterate (+ (0 ,-1))

right :: Coord -> Iterator Coord
right = tail . myIterate (+ (0, 1))

-- Generic methods

isInbound :: (Ord a, Ord b) => Array (a, b) c -> (a, b) -> Bool
isInbound hm (i, j) = i0 <= i && i <= i1 && j0 <= j && j <= j1
  where ((i0, j0), (i1, j1)) = bounds hm

myIterate :: (a -> a) -> a -> [a]
myIterate f a0 = a0 : myIterate f (f a0)

toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing

flatten :: Foldable t => [t a] -> [a]
flatten = Data.Foldable.concatMap Data.Foldable.toList


--- OPT ME: Possibly use a mutable array, let be lazy and not too, and hope
--- haskell lazy evaluation does the work for us

-- import qualified Data.Array.IO as IA

-- data Direction = Top | Bot | Left | Right deriving (Show, Eq, Enum, Ord)

-- type VisibleFrom = IA.IOArray Direction Maybe Bool

-- type VisibilityMap = Array (Int, Int) VisibleFrom

-- newVisibleFrom =
-- newVisibilityMap -> (Int, Int) -> MVisbilityMap
