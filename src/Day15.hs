module Day15 (
  day15part1,
  day15part2,
  merge,
  readSensor,
  holesInRanges,
  holesInBound,
  insert
) where

import Lib(submatches, sliding)
import Data.Set(difference)
import Data.Maybe(maybe, fromJust, isJust)
import Control.Exception (assert)
import qualified Data.Set as Set


-- sensors
--   * knows its position
--   * can determine the position of a beacon precisely
--   * locks to the closest beacon (by manhattan distance)
--     assumption: there's never a tie where 2 beacons have same distance from sensor


-- beacons
-- manhattan distance

type Coord = (Int, Int)

type Range = (Int, Int)

data Sensor = Sensor { sensorPos :: Coord, beaconPos :: Coord, manDist :: Int } deriving (Show)

merge :: Range -> Range -> (Range, Maybe Range)
merge a@(a0, a1) b@(b0, b1)
  | a0 > b0 = merge b a
  | a `lt` b = (a, Just b)
  | a1 < b1 = ((a0, b1), Nothing)
  | otherwise = (a, Nothing)

lt :: Range -> Range -> Bool
lt (_, a1) (b0, _) = a1 < b0 - 1

insert :: Range -> [Range] -> [Range]
insert r [] = [r]
insert r0 (r1:t)
  | r0 `lt` r1 = r0 : r1 : t
  | otherwise = case merge r0 r1 of
    (r', Just r'') -> r' : insert r'' t
    (r', Nothing) -> insert r' t


coordRegex = "x=(-?[[:digit:]]+), y=(-?[[:digit:]]+)"
sensorRegex = "Sensor at " ++ coordRegex ++ ": closest beacon is at " ++ coordRegex

man :: Coord -> Coord -> Int
man (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)


readSensor :: String -> Sensor
readSensor str = case submatches sensorRegex str of
  [x0, y0, x1, y1] ->
    let s = (read x0, read y0)
        b = (read x1, read y1)
    in Sensor { sensorPos = s, beaconPos = b, manDist = man s b }
  _ -> error $ "unable to parse" ++ show str


noBeaconXs :: Sensor -> Int -> [Int]
noBeaconXs sensor y = maybe [] (\ (x0, x1) -> [x0..x1]) (noBeaconRange sensor y)

noBeaconRange :: Sensor -> Int -> Maybe Range
noBeaconRange Sensor { sensorPos = (sx, sy), manDist = md } y
  | dy > md = Nothing
  | otherwise = Just (sx - nbPoints, sx + nbPoints)
  where dy = abs (sy - y)
        nbPoints = md - dy

-- OPT ME: Use Range instead of points
nbNoBeaconPos :: [Sensor] -> Int -> Int
nbNoBeaconPos sensors y = Set.size ( noXs `difference` beaconXs )
  where noXs = Set.fromList . concatMap (`noBeaconXs` y) $ sensors
        beaconXs = Set.fromList [bx | Sensor{beaconPos = (bx, by)} <- sensors , by == y]

day15part1 :: [String] -> Int -> Int
day15part1 sensors y = (`nbNoBeaconPos` y) . fmap readSensor $ sensors


------ Part 2
-- (x, y) E [0, 4M]

noBeaconRanges :: [Sensor] -> Int -> [Range]
noBeaconRanges sensors y = foldl noRanges [] sensors
  where noRanges rgs s = concatRanges rgs (noBeaconRange s y)
        concatRanges rgs (Just r) = insert r rgs
        concatRanges rgs Nothing  =  rgs

holesInRanges :: [Range] -> [Int]
holesInRanges = concatMap holes . sliding 2
  where holes ((x0, x1):(y0, _):_) = assert (x1 < y0 - 1) [x1 + 1 .. y0 - 1]

boundRanges :: Int -> Int -> [Range] -> [Range]
boundRanges from to = insert minRange . insert maxRange
  where minRange = (minBound :: Int, from - 1)
        maxRange = (to + 1, maxBound :: Int)

holesInBound :: Int -> Int -> [Sensor] -> Int -> [Int]
holesInBound from to sensors y =  holesInRanges (boundRanges from to (noBeaconRanges sensors y))

day15part2 :: [String] -> Int -> Int -> (Int, Coord)
day15part2 sensorStrs from to = (tuned . head) [(fromJust x, y) | y <- [from..to], let x = hole y, isJust x ]
  where sensors = fmap readSensor sensorStrs
        hole = firstHole . holesInBound from to sensors
        firstHole [] = Nothing
        firstHole [h] = Just h
        firstHole _ = error "Only expecting one hole"
        tuned c = (tuningFreq c, c)


tuningFreq :: Coord -> Int
tuningFreq (x, y) = x*4000000 + y

sensorStrs1 = [
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3" ]
