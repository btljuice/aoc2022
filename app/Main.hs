module Main (main) where

import Prelude
import System.Directory
import Day1
import Day2
import Day3
import Day4
import Day5
import qualified Day5 as PermutationOrder

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


day4Solution = do
  ls <- readLines "day4.txt"
  putStrLn $ "day4 part 1 = " ++ show (day4Part1 ls)
  putStrLn $ "day4 part 2 = " ++ show (day4Part2 ls)

day5Solution = do
  ls <- readLines "day5.txt"
  putStrLn $ "day5 part 1 = " ++ show (day5 PermutationOrder.Inverted ls day5InitialCrates)
  putStrLn $ "day5 part 2 = " ++ show (day5 PermutationOrder.Same ls day5InitialCrates)



main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "current directory" ++ show cwd
  -- day1Solution
  -- day2Solution
  -- day3Solution
  -- day4Solution
  day5Solution