module Main (main) where

import Prelude
import System.Directory
import Day1
import Day2(readBout, gamePoints)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

day1Solution = do
  ls <- readLines "day1.txt"
  let calories =  caloriesPerElf . readCalories $ ls
  let day1 = sum $ topCalories 3 calories
  putStrLn $ "day1 = " ++ show day1


day2Solution = do
  ls <- readLines "day2.txt"
  let game = map readBout ls
  let points = gamePoints game
  putStrLn $ "day2 part 1 = " ++ show points



main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "current directory" ++ show cwd
  -- day1Solution
  day2Solution