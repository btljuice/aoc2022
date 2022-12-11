{-# LANGUAGE InstanceSigs #-}
module Day11 (
  Monkey(..),
  rounds,
  WorryLevel,
  day11part1,
) where

import Lib (replace)
import Data.List (transpose, sortOn)
import qualified Data.Ord as Ordering

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


oneRound :: [Monkey] -> [Monkey]
oneRound monkeys = foldl throwAllItems monkeys [0..(length monkeys - 1)]

-- N.B. There's probably some Applicative typeclass method that enables doing that
rounds :: Int -> [Monkey] -> [[Monkey]]
rounds n monkeys = scanl (\ ms f -> f ms) monkeys (replicate n oneRound)

day11part1 :: [Monkey] -> Int
day11part1 = product . take 2 . sortOn negate . map sum .transpose . map toCounts . rounds (20-1) -- no need to process last round
  where toCounts = map (length . items)
