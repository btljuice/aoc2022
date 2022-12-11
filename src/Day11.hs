{-# LANGUAGE InstanceSigs #-}
module Day11 (
  Monkey(..),
  rounds,
  WorryLevel,
  day11part1,
  day11Monkeys,
) where

import Lib (replace)
import Data.List (transpose, sortOn)

---- Monkey interface

type WorryLevel = Int

data Monkey = Monkey {
  items :: [WorryLevel], -- starting items
  update :: WorryLevel -> Int, -- updates worry level after inspection
  throwTo :: WorryLevel -> Int -- to which monkey to throw the item to.
}

instance Show Monkey where
  show :: Monkey -> String
  show Monkey{items = items'}= "Monkey { " ++ show items' ++ " }"

-- Adds item at end of list
addItem :: Monkey -> WorryLevel -> Monkey
addItem m w = m { items = items m ++ [w] }

popItem :: Monkey -> (Monkey, WorryLevel)
popItem Monkey{ items = [] } = error "Monkey has no item"
popItem m@Monkey{items = w:rest } = (m{items = rest}, w)

hasItem :: Monkey -> Bool
hasItem Monkey{ items = [] } = False
hasItem _ = True

noItem :: Monkey -> Bool
noItem = not . hasItem

-- Throws next item in monkey's list
throwItem :: Monkey -> (Monkey, (Int, WorryLevel))
throwItem Monkey{items = []} = error "Monkey needs an non-empty list"
throwItem m@Monkey{items = w:rest } = (m {items = rest}, (i, w'))
  where w' = relief . update m $ w
        i  = throwTo m w'

-- after inspection of an item: Divides worry by 3, rounds down
relief :: WorryLevel -> WorryLevel
relief  = (`div` 3)



-- Assumes monkeys !! i at least one item
throwOneItem :: Int -> [Monkey] -> [Monkey]
throwOneItem i monkeys = replace [(i, m'), (j, m1)] monkeys
  where m = monkeys !! i
        (m', (j, w)) = throwItem m
        m1 = addItem (monkeys !! j) w

throwAllItems :: [Monkey] -> Int -> [Monkey]
throwAllItems monkeys i
  | noItem m = monkeys
  | otherwise = throwAllItems (throwOneItem i monkeys) i
  where m = monkeys !! i


oneRound :: [Monkey] -> ([Monkey], [Int])
oneRound monkeys = foldl foldFn (monkeys, [] :: [Int])  [0..(length monkeys - 1)]
  where foldFn (ms, counts) i = (throwAllItems ms i, counts ++ [countItems ms i])
        countItems ms i = length . items $ ms !! i

-- N.B. There's probably some Applicative typeclass method that enables doing that
rounds :: Int -> [Monkey] -> [([Monkey], [Int])]
rounds n monkeys = scanl (\ (ms, _) f -> f ms) (monkeys, replicate 0 (length monkeys)) (replicate n oneRound)

day11part1 :: [Monkey] -> Int
day11part1 = product . take 2 . sortOn negate . map sum . transpose . map snd . rounds 20 -- no need to process last round

day11Monkeys :: [Monkey]
day11Monkeys = [
  Monkey{
    items = [53, 89, 62, 57, 74, 51, 83, 97],
    update = (* 3),
    throwTo = divisibleBy 13 1 5
  },
  Monkey{
    items = [85, 94, 97, 92, 56],
    update = (+ 2),
    throwTo = divisibleBy 19 5 2
  },
  Monkey{
    items = [86, 82, 82],
    update = (+ 1),
    throwTo = divisibleBy 11 3 4
  },
  Monkey{
    items = [94, 68],
    update = (+ 5),
    throwTo = divisibleBy 17 7 6
  },
  Monkey{
    items = [83, 62, 74, 58, 96, 68, 85],
    update = (+ 4),
    throwTo = divisibleBy 3 3 6
  },
  Monkey{
    items = [50, 68, 95, 82],
    update = (+ 8),
    throwTo = divisibleBy 7 2 4
  },
  Monkey{
    items = [75],
    update = (* 7),
    throwTo = divisibleBy 5 7 0
  },
  Monkey{
    items = [92, 52, 85, 89, 68, 82],
    update = \w -> w * w,
    throwTo = divisibleBy 2 0 1
  } ]
  where divisibleBy d t f w = if w `mod` d == 0 then t else f