module Day16 (
  readValve,
  Valve(..),
  Label(..),
  mkValve,
  mkLabel,
  travelCosts,
  mkTravelCosts,
  mkFlowRates,
  flowRates,
) where

import Lib(submatches3, shortestPath)
import Data.List.Split(splitOn)
import Data.Array(Array, Ix, (//), (!))
import qualified Data.Array as Array
import qualified Text.Read
import Data.Hashable(Hashable, hashWithSalt)

-- 30 minutes before volcano erupts
-- valves
-- valve have flow rates
-- valves  are connected by tunnels

type FlowRate = Int
type Cost = Int

-- Label type. Int is the numerical representation of "[A-Z][A-Z]"
-- 2 char has 26*26 = 676 possibilities
-- TODO Assign a unique id to each valve to further reduce dimension
newtype Label = Label Int deriving (Eq, Ord, Ix)

instance Bounded Label where
  minBound = mkLabel "AA"
  maxBound = mkLabel "ZZ"

instance Hashable Label where
  hashWithSalt s (Label i) = hashWithSalt s i


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

-- Valve type

data Valve = Valve Label FlowRate [Label] deriving (Show, Eq)

mkValve :: String -> Int  -> [String] -> Valve
mkValve from flowRate tos = Valve (read from) flowRate (fmap read tos)


posNumberRegex ="[[:digit:]]+"
labelRegex = "[A-Z][A-Z]"
valveRegex = "Valve (" ++ labelRegex ++") has flow rate=(" ++ posNumberRegex ++ "); tunnels? leads? to valves? (.+)"

readValve :: String -> Valve
readValve str = Valve (read valveLabel) (read flowRate) labels
  where (valveLabel, flowRate, lbls) = submatches3 valveRegex str
        labels = fmap read . splitOn ", " $ lbls

type TravelCosts = Array (Label, Label) Cost
type FlowRates = Array Label FlowRate

mkFlowRates :: FlowRates
mkFlowRates = Array.listArray dim (repeat (maxBound :: FlowRate))
  where dim = (minBound :: Label, maxBound :: Label)

flowRates :: [Valve] -> FlowRates
flowRates valves = mkFlowRates  // fmap (\ (Valve l fr _) -> (l, fr)) valves


mkTravelCosts :: TravelCosts
mkTravelCosts = Array.listArray dim (repeat (maxBound :: Cost))
  where dim = ((minBound :: Label, minBound :: Label), (maxBound :: Label, maxBound :: Label))


non0Labels :: [Valve] -> [Label]
non0Labels valves = [from | Valve from rate _ <- valves, rate > 0]

shortestPaths :: [Valve] -> [[Label]]
shortestPaths valves = filter (\ p -> length p > 1) ( shortestPath edges <$> labels <*> labels )
        -- ASSUMPTION: The valves description is bi-directional
  where edges = [(to,  from) | Valve from _ tos <- valves, to <- tos]
        labels = startLabel : non0Labels valves

travelCosts :: [Valve] -> TravelCosts
travelCosts valves  = mkTravelCosts // costs valves
  where costs = fmap pathCost . shortestPaths
        pathCost p = ((head p, last p), length p - 1)

pathFlowRate :: FlowRates -> TravelCosts ->  [Label]  -> Int -> Int
pathFlowRate _  _ _ 1 = 0 -- takes 1 minute to open a valve
pathFlowRate _  _ _ 0 = 0
pathFlowRate _  _ [] _ = 0
pathFlowRate _  _ [_] _ = 0
pathFlowRate frs tcs (p0:p1:t) m
  | m' <= 0 = 0
  | otherwise = m'*fr + pathFlowRate frs tcs (p1:t) m'
  where tc = tcs ! (p0, p1)
        fr = frs ! p1
        m' = m - tc - 1  -- 1 minute removed because valve is opening

-- bestPath valves =
--   where paths :: [[Label]]
--         paths = Data.Permutations . non0Labels $ valves
--         costs = travelCosts valves

-- cost
-- 1 minute opening a valve
-- 1 minute moving in a tunnel
-- many valves are a flow 0, therefore no reason to open them
-- Start at AA
-- pressure released = remaining time * flowrate

-- valves w/ 0 are just noise actually
