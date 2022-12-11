{-# LANGUAGE InstanceSigs #-}
module Day10 (
  day10part1,
  registerValues,
  readInstructions,
  signalStrengths,
  cyclesOfInterest,
  sequentialInstructions,
) where

import Text.Read ( Read(readPrec), ReadPrec, (+++) )
import Data.Functor (($>))
import Lib(pString)

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

-- Register Values at the *end* of cycle
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

