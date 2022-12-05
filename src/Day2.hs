module Day2
  ( readRPS,
    readBout,
    readBout2,
    gamePoints,
    gamePoints2,
    -- (..) to import "data constructor"s
    RPS (..),
    Outcome (..),
  )
where

data RPS = R | P | S deriving (Eq, Show)
data Outcome = W | D | L deriving (Eq, Show)

onlyTwoWords :: String -> (String, String)
onlyTwoWords l = case words l of
  [a, b] -> (a, b)
  _ -> error $ "Line as not 2 words :" ++ show l

readBout :: String -> (RPS, RPS)
readBout line = (readRPS l, readRPS r)
  where (l, r) = onlyTwoWords line

readBout2 :: String -> (RPS, Outcome)
readBout2 line = (readRPS l, readOutcome r)
  where (l, r) = onlyTwoWords line


readRPS :: String -> RPS
readRPS "A" = R
readRPS "B" = P
readRPS "C" = S
readRPS "X" = R
readRPS "Y" = P
readRPS "Z" = S
readRPS s = error ("Not a valid RPS character: " ++ s)

readOutcome :: String -> Outcome
readOutcome "X" = L
readOutcome "Y" = D
readOutcome "Z" = W
readOutcome s = error ("Not a valid Outcome character: " ++ s)

inferRPS :: RPS -> Outcome -> RPS
inferRPS R L = S
inferRPS R W = P
inferRPS P L = R
inferRPS P W = S
inferRPS S L = P
inferRPS S W = R
inferRPS x D = x


winPoints :: RPS -> RPS -> Int
winPoints S R = 6
winPoints R R = 3
winPoints P R = 0
winPoints R P = 6
winPoints P P = 3
winPoints S P = 0
winPoints P S = 6
winPoints S S = 3
winPoints R S = 0

shapePoints :: RPS -> Int
shapePoints R = 1
shapePoints P = 2
shapePoints S = 3

boutPoints :: RPS -> RPS -> Int
boutPoints l r = winPoints l r + shapePoints r


gamePoints :: [(RPS, RPS)] -> Int
gamePoints = sum . map (uncurry boutPoints)

gamePoints2 :: [(RPS, Outcome)] -> Int
gamePoints2 =  gamePoints . map (\(l, o) -> (l, inferRPS l o))
