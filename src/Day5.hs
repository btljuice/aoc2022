module Day5
  ( day5,
    day5InitialCrates,
    permuteCrates,
    permuteCrate,
    applyInstructions,
    readInstruction,
    Crates,
    Instruction,
    PermutationOrder (..),
  )
where

import GHC.Arr (Array, (!), (//), array)
import Text.Regex.TDFA
import Data.Foldable (toList)

type Instruction = (Int, Int, Int)
type Crates a = Array Int [a]
data PermutationOrder = Same | Inverted deriving (Enum, Eq)

day5 :: PermutationOrder -> [String] -> Crates Char -> String
day5 po instructions = topCrates . applyInstructions po (map readInstruction instructions)


topCrates :: Crates a -> [a]
topCrates crates = [head c | c <- toList crates]


orderFnc :: PermutationOrder -> ([a] -> [a])
orderFnc Same = id
orderFnc Inverted = reverse


-- permutes n crates from left stack to right stack
permuteCrate :: PermutationOrder -> Int -> [a] -> [a] -> ([a], [a])
permuteCrate po n l r = (drop n l, (orderFnc po . take n $ l) ++ r)

permuteCrates :: PermutationOrder -> Instruction -> Crates a -> Crates a
permuteCrates po (n, i, j) crates = crates // [(i, l), (j, r)]
  where (l, r) = permuteCrate po n (crates ! i) (crates ! j)

applyInstructions :: PermutationOrder -> [Instruction] -> Crates a -> Crates a
applyInstructions po instructions crates = foldl (flip (permuteCrates po)) crates instructions


-- "move n from l to r". Cheating a bit here as i'm only fetching the numbers
readInstruction :: String -> Instruction
readInstruction s = only3 ( getAllTextMatches (s =~ "[0-9]+") :: [String] )
  where only3 [n, i, j] = (read n :: Int, read i :: Int, read j :: Int)
        only3 _ = error $ "Line is not an instruction with 3 number: " ++ s


-- Lame manual conversion of day5 inputs
--                 [B] [L]     [J]
--             [B] [Q] [R]     [D] [T]
--             [G] [H] [H] [M] [N] [F]
--         [J] [N] [D] [F] [J] [H] [B]
--     [Q] [F] [W] [S] [V] [N] [F] [N]
-- [W] [N] [H] [M] [L] [B] [R] [T] [Q]
-- [L] [T] [C] [R] [R] [J] [W] [Z] [L]
-- [S] [J] [S] [T] [T] [M] [D] [B] [H]
--  1   2   3   4   5   6   7   8   9
day5InitialCrates :: Crates Char
day5InitialCrates = array (1, 9) [
  (1, ['W', 'L', 'S']),
  (2, ['Q', 'N', 'T', 'J']),
  (3, ['J', 'F', 'H', 'C', 'S']),
  (4, ['B', 'G', 'N', 'W', 'M', 'R', 'T']),
  (5, ['B', 'Q', 'H', 'D', 'S', 'L', 'R', 'T']),
  (6, ['L', 'R', 'H', 'F', 'V', 'B', 'J', 'M']),
  (7, ['M', 'J', 'N', 'R', 'W', 'D']),
  (8, [ 'J', 'D', 'N', 'H', 'F', 'T', 'Z', 'B']),
  (9, [ 'T', 'F', 'B', 'N', 'Q', 'L', 'H']) ]
