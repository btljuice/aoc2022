module Lib
  ( pal,
    someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pal :: String -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False