module Day16 (
  readValve,
  Valve(..),
  Label(..),
  mkValve,
  mkLabel,
  shortestPaths
) where

import Lib(submatches3, shortestPath)
import Data.List.Split(splitOn)
import Data.Array(Array, Ix)
import qualified Text.Read

-- 30 minutes before volcano erupts
-- valves
-- valve have flow rates
-- valves  are connected by tunnels

type Cost = Int

-- TODO convert to data Label = Label Int
-- make Read and Show for 2 chars
data Label = Label Int deriving (Eq, Ord, Ix)-- 2 Char has 26*26 = 676 possibilities

instance Show Label where
  show (Label i) = [a, b]
    where a = toEnum (i `div` 26 + fromEnum 'A') :: Char
          b = toEnum (i `mod` 26 + fromEnum 'A') :: Char

instance Read Label where
  readPrec = do
    a <- Text.Read.get
    b <- Text.Read.get
    let toInt c = fromEnum c - fromEnum 'A'
    return (Label (26 * toInt a + toInt b))

mkLabel :: String ->  Label
mkLabel = read

startLabel = mkLabel "AA"


type FlowRate = Int

data Valve = Valve Label FlowRate [Label] deriving (Show, Eq)

type TravelCost = Array Label [(Label, Int)]


mkValve :: String -> Int  -> [String] -> Valve
mkValve from flowRate tos = Valve (read from) flowRate (fmap read tos)

-- read valve function

posNumberRegex ="[[:digit:]]+"
labelRegex = "[A-Z][A-Z]"
valveRegex = "Valve (" ++ labelRegex ++") has flow rate=(" ++ posNumberRegex ++ "); tunnels? leads? to valves? (.+)"

readValve :: String -> Valve
readValve str = Valve (read valveLabel) (read flowRate) labels
  where (valveLabel, flowRate, lbls) = submatches3 valveRegex str
        labels = fmap read . splitOn ", " $ lbls

shortestPaths :: [Valve] -> [[Label]]
shortestPaths valves = filter (\ p -> length p > 1) ( shortestPath edges <$> non0Labels <*> non0Labels )
  where edges = concat [[(from, to), (to,  from)] | Valve from _ tos <- valves, to <- tos]
        non0Labels = startLabel : [from | Valve from rate _ <- valves, rate > 0]

-- travelCosts :: [Valve] -> TravelCost

-- cost
-- 1 minute opening a valve
-- 1 minute moving in a tunnel
-- many valves are a flow 0, therefore no reason to open them
-- Start at AA
-- pressure released = remaining time * flowrate

-- valves w/ 0 are just noise actually
