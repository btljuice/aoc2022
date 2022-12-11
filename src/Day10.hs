{-# LANGUAGE InstanceSigs #-}
module Day10 (
  day10part1,
  registerValues,
  readInstructions,
  signalStrengths,
  cyclesOfInterest,
  sequentialInstructions,
  drawImage,
  day10part2,
) where

import Text.Read ( Read(readPrec), ReadPrec, (+++) )
import Data.Functor (($>))
import Lib(pString)
import Data.List.Split (chunksOf)

data Instruction = Add Int | Noop deriving (Show)

instance Read Instruction where
  readPrec :: ReadPrec Instruction
  readPrec = noop +++ addx
    where noop = pString "noop" $> Noop
          addx = Add <$> (pString "addx " *> readPrec)

------- part 1
-- addx (2 cycles delay)
-- noop (1 cycle delay)
-- register X = 0 at start of cycle 1

initialRegisterValue :: Int
initialRegisterValue = 1

cyclesOfInterest :: [Int]
cyclesOfInterest = [20, 60, 100, 140, 180, 220]

sequentialInstructions :: [Instruction] -> [Instruction]
sequentialInstructions = concatMap delay
  where delay Noop = [Noop]
        delay a@(Add _) = [Noop, a]

-- Register Values during cycles [1..]. Instruction is applied at end of cycle, but scanl adds the initial value to the
-- list
registerValues :: [Instruction] -> [Int]
registerValues instructions = scanl (+) initialRegisterValue incs
  where incs = map inc instructions
        inc Noop = 0
        inc (Add i) = i

signalStrengths :: [Int] -> [Int]
signalStrengths values = [r * i | (r, i) <- values `zip` [1..], i `elem` cyclesOfInterest]

day10part1 :: [String] -> Int
day10part1 = sum . signalStrengths . registerValues . sequentialInstructions . readInstructions

readInstructions :: [String] -> [Instruction]
readInstructions = map read


-- Part 2 CRT Screen
drawImage :: [Int] -> [Char]
drawImage registers = [pixelChar r p | (r, p) <- registers `zip` pixelPos]
  where pixelPos = cycle [0..39]
        pixelChar s p = if p-1 <= s && s <= p+1 then '#' else '.'

day10part2 :: [String] -> [String]
day10part2 = chunksOf 40 . take (6*40) . drawImage  . registerValues  . sequentialInstructions . readInstructions