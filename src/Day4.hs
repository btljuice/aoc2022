module Day4
  (
    day4Part1,
    day4Part2
  )
where

import Data.List.Split (splitOn)

day4Part1 :: [String] -> Int
day4Part1 = sum . map (fromEnum .  eitherContains . readRanges)
  where eitherContains (rg0, rg1) = rg0 `contains` rg1 || rg1 `contains` rg0

day4Part2 :: [String] -> Int
day4Part2 = sum . map (fromEnum .  uncurry overlaps . readRanges)

type Range = (Int, Int)

-- parse ranges separated by ,
readRanges :: String -> (Range, Range)
readRanges s = only2 . splitOn "," $ s
  where only2 [l, r] = (readRange l, readRange r)
        only2 _ = error $ "Only 2 ranges per line: " ++ s

-- parse range separated by -
readRange :: String -> Range
readRange s = only2 . splitOn "-" $ s
  where only2 [a, b] = (read a :: Int, read b :: Int)
        only2 _ = error $ "A range should only contain 2 numbers: " ++ s

contains :: Range -> Range -> Bool
contains (l0, r0) (l1, r1) = l0 <= l1 && r1 <= r0

overlaps :: Range -> Range -> Bool
overlaps (l0, r0) (l1, r1)
  | l0 < l1 = r0 >= l1
  | otherwise = r1 >= l0


