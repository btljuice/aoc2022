module Main (main) where

import Prelude
import System.Directory
import Day1
import Day2
import Day3

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
  let game2 = map readBout2 ls
  let points2 = gamePoints2 game2
  putStrLn $ "day2 part 1 = " ++ show points
  putStrLn $ "day2 part 2 = " ++ show points2

day3Solution = do
  ls <- readLines "day3.txt"
  putStrLn $ "day3 part 1 = " ++ show (day3Part1 ls)
  putStrLn $ "day3 part 2 = " ++ show (day3Part2 ls)




main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "current directory" ++ show cwd
  -- day1Solution
  -- day2Solution
  day3Solution