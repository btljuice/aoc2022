{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Prelude
import System.Directory
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import qualified Day5 as PermutationOrder
import Day7
import Day8 (day8part1, day8part2)
import Day9 (day9part1, day9part2)
import Day10 (day10part1, day10part2)
import Day11(day11part1, day11Monkeys, Monkey (shouldRelief), day11part2)
import Day12(day12part1, day12part2)
import Day13(day13part1, day13part2)
import Day14(day14part1, day14part2)
import Day15(day15part1, day15part2)
import Day16(day16part1)

import Data.List(sort)
import Data.Tuple.Extra (snd3)


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

day1Solution :: IO ()
day1Solution = do
  ls <- readLines "day1.txt"
  let calories =  caloriesPerElf . readCalories $ ls
  let day1 = sum $ topCalories 3 calories
  putStrLn $ "day1 = " ++ show day1


day2Solution :: IO ()
day2Solution = do
  ls <- readLines "day2.txt"
  let game = map readBout ls
  let points = gamePoints game
  let game2 = map readBout2 ls
  let points2 = gamePoints2 game2
  putStrLn $ "day2 part 1 = " ++ show points
  putStrLn $ "day2 part 2 = " ++ show points2

day3Solution :: IO ()
day3Solution = do
  ls <- readLines "day3.txt"
  putStrLn $ "day3 part 1 = " ++ show (day3Part1 ls)
  putStrLn $ "day3 part 2 = " ++ show (day3Part2 ls)


day4Solution :: IO ()
day4Solution = do
  ls <- readLines "day4.txt"
  putStrLn $ "day4 part 1 = " ++ show (day4Part1 ls)
  putStrLn $ "day4 part 2 = " ++ show (day4Part2 ls)

day5Solution :: IO ()
day5Solution = do
  ls <- readLines "day5.txt"
  putStrLn $ "day5 part 1 = " ++ show (day5 PermutationOrder.Inverted ls day5InitialCrates)
  putStrLn $ "day5 part 2 = " ++ show (day5 PermutationOrder.Same ls day5InitialCrates)

day6Solution :: IO ()
day6Solution = do
  ls <- readLines "day6.txt"
  let content = head ls
  putStrLn $ "day6 part 1 = " ++ show (firstMarkerPos 4 content)
  putStrLn $ "day6 part 2 = " ++ show (firstMarkerPos 14 content)

day7Solution :: IO ()
day7Solution = do
  ls <- readLines "day7.txt"
  putStrLn $ "day7 part 1 = " ++ (show . day7part1 $ tail ls)
  putStrLn $ "day7 part 2 = " ++ (show . day7part2 $ tail ls)
  putStrLn $ "day7 all values = " ++ show [s | s <- sort (day7AllValues (tail ls)) , s >= 8381165]

day8Solution :: IO ()
day8Solution = do
  ls <- readLines "day8.txt"
  putStrLn $ "day8 part 1 = " ++ (show . day8part1 $ ls)
  putStrLn $ "day8 part 2 = " ++ (show . day8part2 $ ls)

day9Solution :: IO ()
day9Solution = do
  ls <- readLines "day9.txt"
  putStrLn $ "day9 part 1 = " ++ (show . length . day9part1 $ ls)
  putStrLn $ "day9 part 2 = " ++ (show . length . day9part2 $ ls)

day10Solution :: IO ()
day10Solution = do
  ls <- readLines "day10.txt"
  putStrLn $ "day10 part 1 = " ++ (show . day10part1 $ ls)
  putStrLn "day10 part 2 ="
  mapM_ putStrLn (day10part2 ls)

day11Solution :: IO ()
day11Solution = do
  putStrLn $ "day11 part 1 = " ++ (show . day11part1 $ day11Monkeys)
  putStrLn $ "day11 part 2 = " ++ (show . day11part2 $ map (\ m  -> m{shouldRelief = False}) day11Monkeys)

day12Solution :: IO ()
day12Solution = do
  ls <- readLines "day12.txt"
  putStrLn $ "day12 part 1 = " ++ (show . day12part1 $ ls)
  putStrLn $ "day12 part 2 = " ++ (show . day12part2 $ ls)

day13Solution :: IO ()
day13Solution = do
  ls <- readLines "day13.txt"
  putStrLn $ "day13 part 1 = " ++ (show . day13part1 $ ls)
  putStrLn $ "day13 part 2 = " ++ (show . day13part2 $ ls)

day14Solution :: IO ()
day14Solution = do
  ls <- readLines "day14.txt"
  putStrLn $ "day14 part 1 = " ++ (show . snd3 . day14part1 $ ls)
  putStrLn $ "day14 part 2 = " ++ (show . snd3 . day14part2 $ ls)

day15Solution :: IO ()
day15Solution = do
  ls <- readLines "day15.txt"
  putStrLn $ "day15 part 1 = " ++ show (day15part1 ls 2000000)
  putStrLn $ "day15 part r = " ++ show (day15part2 ls 0 4000000)

day16Solution :: IO ()
day16Solution = do
  ls <- readLines "day16.txt"
  putStrLn $ "day16 part 1 = " ++ show (day16part1 ls)



main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "current directory" ++ show cwd
  -- day1Solution
  -- day2Solution
  -- day3Solution
  -- day4Solution
  -- day5Solution
  -- day6Solution
  -- day7Solution
  -- day8Solution
  -- day9Solution
  -- day10Solution
  -- day11Solution
  -- day12Solution
  -- day13Solution
  -- day14Solution
  -- day15Solution
  day16Solution
