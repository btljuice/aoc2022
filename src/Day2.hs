module Day2
  ( readRPS,
    setPoints,
    RPS (..)
  )
where

data RPS = R | P | S

readRPS :: String -> RPS
readRPS "A" = R
readRPS "B" = P
readRPS "C" = S
readRPS "X" = R
readRPS "Y" = P
readRPS "Z" = S
readRPS s = error ("Not a valid RPS character: " ++ s)

winPoints :: RPS -> RPS -> Int
winPoints R S = 6
winPoints R R = 3
winPoints R P = 0
winPoints P R = 6
winPoints P P = 3
winPoints P S = 0
winPoints S P = 6
winPoints S S = 3
winPoints S R = 0

shapePoints :: RPS -> Int
shapePoints R = 1
shapePoints P = 2
shapePoints S = 3

gamePoints :: RPS -> RPS -> Int
gamePoints l r = winPoints l r + shapePoints l


setPoints :: [(RPS, RPS)] -> Int
setPoints = sum . map (uncurry gamePoints)
