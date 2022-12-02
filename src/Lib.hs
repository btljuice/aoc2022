module Lib
  ( palindrome,
    someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

palindrome :: String -> Bool
palindrome xs
  | xs == reverse xs = True
  | otherwise = False