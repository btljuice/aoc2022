module Day15 (
  day15part1
) where

import Lib(submatches)
import Data.Set(difference)
import qualified Data.Set as Set


-- sensors
--   * knows its position
--   * can determine the position of a beacon precisely
--   * locks to the closest beacon (by manhattan distance)
--     assumption: there's never a tie where 2 beacons have same distance from sensor


-- beacons
-- manhattan distance

type Coord = (Int, Int)

data Sensor = Sensor { sensorPos :: Coord, beaconPos :: Coord, manDist :: Int } deriving (Show)


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
noBeaconXs Sensor { sensorPos = (sx, sy), manDist = md } y
  | dy > md = []
  | otherwise = [sx - nbPoints .. sx + nbPoints]
  where dy = abs (sy - y)
        nbPoints = md - dy

nbNoBeaconPos :: [Sensor] -> Int -> Int
nbNoBeaconPos sensors y = Set.size ( noXs `difference` beaconXs )
  where noXs = Set.fromList . concatMap (`noBeaconXs` y) $ sensors
        beaconXs = Set.fromList [bx | Sensor{beaconPos = (bx, by)} <- sensors , by == y]

day15part1 :: [String] -> Int -> Int
day15part1 sensors y = (`nbNoBeaconPos` y) . fmap readSensor $ sensors
