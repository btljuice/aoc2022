module Main (main) where

import Prelude
import System.Directory
import Day1

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "current directory" ++ show cwd
  content <- readFile "day1.txt"
  let calories =  caloriesPerElf . readCalories  . lines $ content
  let day1 = sum $ topCalories 3 calories
  putStrLn $ "day1 = " ++ show day1