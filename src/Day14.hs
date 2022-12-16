module Day14 (
  day14part1,
  day14part2,
  drawCellArray,
) where

import Lib(onlyTwo, trim, sliding, minmax, showCharArray, isInbound)
import Data.List.Split(splitOn)
import Data.Tuple.Extra(both)
import Text.Regex(mkRegex, splitRegex)
import Data.Array(Array, (//), (!))
import qualified Data.Array as Array
import qualified Data.List as List

-- x -> right ; y -> down
type Coord = (Int, Int)
type Dim = (Coord, Coord)
type Path = [Coord]

data Cell = Rock | Sand | Air | Abyss | Source
type CellArray = Array Coord Cell

isAirOrSource :: Cell -> Bool
isAirOrSource Air = True
isAirOrSource Source = True
isAirOrSource _ = False


cell2char :: Cell -> Char
cell2char Rock = '#'
cell2char Sand = 'o'
cell2char Air  = '.'
cell2char Source = '+'
cell2char Abyss = '~'

source = (500, 0)

coordBounds :: [Coord] -> Dim
coordBounds coords = ((minX, minY), (maxX, maxY))
  where (minX, maxX) = minmax [x | (x, _) <- coords]
        (minY, maxY) = minmax [y | (_, y) <- coords]

mkAirArray :: [Path] -> CellArray
mkAirArray paths = Array.listArray extendDim (repeat Air)
  where dim@((x0, y0), (x1, y1)) = coordBounds ( source : concat paths )
        dy = y1 + 2 - y0
        x0' = min x0 (500 - dy - 1)
        x1' = max x1 (500 + dy + 1)
        extendDim = ((x0', y0), (x1', y1 + 2))

pathCoords :: Path -> [Coord]
pathCoords = concatMap (uncurry lineCoords . onlyTwo) . sliding 2

lineCoords :: Coord -> Coord -> [Coord]
lineCoords (x0, y0) (x1, y1)
  | isVertical   = [(x0, y ) | y <- [minY..maxY]]
  | isHorizontal = [(x,  y0) | x <- [minX..maxX]]
  | otherwise = error "Unexpected. Line should either be vertical or horizontal"
  where isVertical   = x0 == x1
        isHorizontal = y0 == y1
        (minX, maxX) = if x0 < x1 then (x0, x1) else (x1, x0)
        (minY, maxY) = if y0 < y1 then (y0, y1) else (y1, y0)


readPath :: String -> Path
readPath = fmap (readCoord . trim) . splitRegex (mkRegex "->")

readCoord :: String -> Coord
readCoord = both read . onlyTwo . splitOn ","


day14part1InitArray :: [Path] -> CellArray
day14part1InitArray paths = mkAirArray paths // [(r, Rock) | r <- rockCoords]
                                        // [(source, Source)]
  where rockCoords = concatMap pathCoords paths

day14part2InitArray :: [Path] -> CellArray
day14part2InitArray paths =
  let arr = day14part1InitArray paths
      ((xmin, _), (xmax, ymax)) = Array.bounds arr
      rockBottom = [((x, ymax), Rock) | x <- [xmin..xmax]]
  in arr // rockBottom



samplePaths :: [String]
samplePaths = [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9" ]

-- 1. Sands pour from (500, 0) coordinate
-- 2. Inputs are line of rocks.
-- 3. lines are either horizontal or vertical (invariant)
-- 4. Sand rules
--    one unit at a time. the next unit is not produce until the previous comes to rest.

  -- 1. Have we reach abyss (c is out-of-bound) then return 0, true
  -- 2. check if current cell air. if so go down recursively
  --    check for true
  -- 3. then go left-down recursively.
  --    check for true
  -- 4. the ngo right-down recursively.
  --    return
-- sand falls in the following order:
-- 1. down, down-left, down-right


pourSand :: CellArray -> Coord -> (CellArray, Int, Bool)
pourSand arr c
  | not (isInbound arr c)    = (arr, 0, True) -- We'll hit the Abyss. Return True
  | not (isAirOrSource cell) = (arr, 0, False) -- We've hit a Rock or Sand. Return False
  | abyssDown  = (arr'   // [(c, Abyss)], sandDown, True)
  | abyssLeft  = (arr''  // [(c, Abyss)], sandDown + sandLeft, True)
  | abyssRight = (arr''' // [(c, Abyss)], sandDown + sandLeft + sandRight, True)
  | otherwise  =  (arr''' // [(c,  Sand)], sandDown + sandLeft + sandRight + 1, False)
  where cell = arr ! c
        (arr', sandDown, abyssDown) = pourSand arr (down c)
        (arr'', sandLeft, abyssLeft) = pourSand arr' (downLeft c)
        (arr''', sandRight, abyssRight) = pourSand arr'' (downRight c)

day14part1 :: [String] -> (CellArray, Int, Bool)
day14part1 = flip pourSand source  . day14part1InitArray . fmap readPath

day14part2 :: [String] -> (CellArray, Int, Bool)
day14part2 = flip pourSand source  . day14part2InitArray . fmap readPath

down :: Coord -> Coord
down (x, y) = (x, y + 1)

downLeft :: Coord -> Coord
downLeft (x, y) = (x - 1, y + 1)

downRight :: Coord -> Coord
downRight (x, y) = (x + 1, y + 1)

drawCellArray :: CellArray -> [String]
drawCellArray = List.transpose . showCharArray cell2char

------- super cool evaluate block!
{-|
>>>
-}

