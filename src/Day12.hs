module Day12 (
  readHeightMap,
  findPath,
  day12part1,
  day12part2,
) where

import Data.Array (Array, (!), (//) )
import qualified Data.Array as Array
import Lib
import Data.Maybe (isJust)
import Control.Exception (assert)
import qualified Data.List.Split as Data.List
import qualified Data.Foldable
import GHC.Data.Maybe (fromJust)

-- heightmap
-- elevation : [a-z]
-- current position : S (as a position of a)
-- best signal : E (as a position of z)
-- can move up, down, left, right
-- can step to elevation one higher but not more

type Coord = (Int, Int)

data Dir = U | D | L | R deriving (Show, Read, Enum, Eq, Ord)

type HeightMap = Array Coord Int
type DirInfo = (Dir, Int)
type DirMap = Array Coord (Maybe DirInfo)

forward :: Dir -> Coord -> Coord
forward U (i, j) = (i - 1, j)
forward D (i, j) = (i + 1, j)
forward R (i, j) = (i, j + 1)
forward L (i, j) = (i, j - 1)

drawDir :: Maybe Dir -> Char
drawDir (Just U) = '^'
drawDir (Just D) = 'v'
drawDir (Just L) = '<'
drawDir (Just R) = '>'
drawDir Nothing = '.'


backward :: Dir -> Coord -> Coord
backward U = forward D
backward D = forward U
backward L = forward R
backward R = forward L

initDirMap :: Coord -> DirMap
initDirMap dim = Array.listArray ((1, 1), dim) $ repeat Nothing

normalizeHeight :: Char -> Int
normalizeHeight 'S' = 0
normalizeHeight 'E' = 25
normalizeHeight c = fromEnum c - fromEnum 'a'

readHeightMap :: [String] -> (HeightMap, Coord, Coord)
readHeightMap strs =
  let i = length strs
      j = length (head strs)
      charArr = Array.listArray ((1, 1), (i, j)) (concat strs)
      -- fromJust is like a .get . have to check them out
      (start, _) = fromJust . Data.Foldable.find ((== 'S') . snd) . Array.assocs $ charArr
      (end,   _) = fromJust . Data.Foldable.find ((== 'E') . snd) . Array.assocs $ charArr
  in (fmap normalizeHeight charArr, start, end)


propagatePos :: HeightMap -> DirMap -> Coord -> (DirMap, [Coord])
propagatePos hm dm c =
  assert (isJust (dm ! c)) (dm // newDirs, map fst newDirs )
  where newDirs = flatten [
         reachableFrom hm dm c U,
         reachableFrom hm dm c D,
         reachableFrom hm dm c L,
         reachableFrom hm dm c R ]

reachableFrom :: HeightMap -> DirMap -> Coord -> Dir -> Maybe (Coord, Maybe DirInfo)
reachableFrom hm dm to dir
  | not (isInbound hm from) = Nothing -- coordinate is outside heightmap
  | isJust (dm ! from) = assert (fromDistance <= toDistance + 1) Nothing  -- coordinate has already a direction
  | fromHeight < toHeight - 1 = Nothing -- can't reach `to`
  | otherwise = Just (from, Just (dir, toDistance + 1))
  where from = backward dir to
        toHeight = hm ! to
        fromHeight = hm ! from
        (_, toDistance) = fromJust (dm ! to)
        (_, fromDistance) = fromJust (dm !  from)

-- Let's generate closest path to E by backtracking from E, all the way to S, breadth first search
findPath :: HeightMap -> Coord -> Coord -> DirMap
findPath hm _ end = propagateMap hm dirMap0 [end]
  where dirMap0 = initDirMap (snd . Array.bounds $ hm) // [(end, Just (U, 0))]


propagateMap :: HeightMap -> DirMap -> [Coord] -> DirMap
propagateMap hm dm [] = dm
propagateMap hm dm (h:t) = propagateMap hm dm' (t ++ newPos)
  where (dm', newPos) = propagatePos hm dm h


draw :: DirMap -> [String]
draw = drawArray . fmap (drawDir . fmap fst)

drawArray :: Array Coord Char -> [String]
drawArray arr = Data.List.chunksOf dimJ . Data.Foldable.toList $ arr
  where (_, (_, dimJ)) = Array.bounds arr


day12part1 :: [String] -> Int
day12part1 strs =
  let (hm, start, end) = readHeightMap strs
      dm = findPath hm start end
      (_, distance) = fromJust (dm ! start)
  in distance

day12part2 :: [String] -> Int
day12part2 strs = minimum dirs
  where (hm, start, end) = readHeightMap strs
        dm = findPath hm start end
        dirs = [ snd d | (pos, h) <- Array.assocs hm , h == 0,
                         let maybeD = dm ! pos,
                         d <- Data.Foldable.toList maybeD ]
