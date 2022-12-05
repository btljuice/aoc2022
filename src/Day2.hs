module Day2
  ( readRPS,
    readBout,
    gamePoints,
    -- (..) to import "data constructor"s
    RPS (..)
  )
where

data RPS = R | P | S deriving (Eq, Show)

onlyTwoWords :: String -> (String, String)
onlyTwoWords l = case words l of
  [a, b] -> (a, b)
  _ -> error $ "Line as not 2 words :" ++ show l

readBout :: String -> (RPS, RPS)
readBout line = (readRPS l, readRPS r)
  where (l, r) = onlyTwoWords line


readRPS :: String -> RPS
readRPS "A" = R
readRPS "B" = P
readRPS "C" = S
readRPS "X" = R
readRPS "Y" = P
readRPS "Z" = S
readRPS s = error ("Not a valid RPS character: " ++ s)

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
