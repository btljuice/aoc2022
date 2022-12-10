module Day9 (
  visitedByTail,
  day9part1,
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
type Rope = (Coord, Coord)

initialRope :: Rope
initialRope = ((0,0), (0, 0))

day9part1 :: [String] -> Set Coord
day9part1 =  visitedByTail .  concatMap (\ (d, i) -> replicate i d ) .  readMotions

visitedByTail :: [Dir] -> Set Coord
visitedByTail = fst . foldl visit (Set.singleton . snd $ initialRope, initialRope)

visit :: (Set Coord,  Rope) -> Dir -> (Set Coord, Rope)
visit (visited, rope) d = (Set.insert t' visited, rope')
  where rope' = moveRope d rope
        t' = snd rope'

readMotions :: [String] -> [(Dir, Int)]
readMotions = map readTwoWords

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
moveRope d (h, t) = (h', t')
  where h' = move d h
        t' = closeGap h' t
