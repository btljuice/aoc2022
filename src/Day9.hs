module Day9 (
  visitedByTail,
  day9part1,
  day9part2,
) where

import Lib(readTwoWords)
import Data.Set (Set)
import qualified Data.Set as Set

-- head and tail of rope must always be  (either or)
-- 1. adjacent
-- 2. diagonally adjacent,
-- 3. overlapping (at the same position)

-- When head moves, tail must be updated to respect adjacency invariant above.
-- if tail is on same row or column of head, then it moves along that line
-- else it approaches diagonally to be closer to the head

data Dir = U | D | L | R deriving (Enum, Ord, Eq, Read, Show)
type Coord = (Int, Int)
type Rope = [Coord]

initialRope :: Int -> Rope
initialRope n = replicate n (0, 0)

day9part1 :: [String] -> Set Coord
day9part1 =  visitedByTail (initialRope 2) . readMotions

day9part2 :: [String] -> Set Coord
day9part2 =  visitedByTail (initialRope 10) . readMotions

visitedByTail :: Rope -> [Dir] -> Set Coord
visitedByTail rope = fst . foldl visit (Set.singleton . last $ rope, rope)

visit :: (Set Coord,  Rope) -> Dir -> (Set Coord, Rope)
visit (visited, rope) d = (Set.insert t' visited, rope')
  where rope' = moveRope d rope
        t' = last rope'

readMotions :: [String] -> [Dir]
readMotions = concatMap ((\ (d, i) -> replicate i d ) . readTwoWords)

move :: Dir -> Coord -> Coord
move U (x, y) = (x,     y + 1)
move D (x, y) = (x,     y - 1)
move L (x, y) = (x - 1, y    )
move R (x, y) = (x + 1, y    )

closeGap :: Coord -> Coord -> Coord
closeGap (hx, hy) (tx, ty)
  | shouldClose = (tx + dx, ty + dy)
  | otherwise = (tx, ty)
  where
        shouldClose = abs (hx - tx) > 1 || abs (hy - ty) > 1
        dx = signum (hx - tx)
        dy = signum (hy - ty)

moveRope :: Dir -> Rope -> Rope
moveRope d rope = closeGaps (h':tail rope)
   where h' = move d $ head rope

closeGaps :: Rope -> Rope
closeGaps [] = []
closeGaps [x] = [x]
closeGaps (h0:h1:t) = h0 : closeGaps (closeGap h0 h1 : t)
