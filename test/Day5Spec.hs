module Day5Spec (spec) where

import Test.Hspec
import Prelude

import GHC.Arr (Array, array)
import Day5

crates :: Crates Char
crates  = array (1, 3)  [
  (1, ['N', 'Z']),
  (2, ['D', 'C', 'M']),
  (3, ['P']) ]


instructions :: [Instruction]
instructions = [
  (1, 2, 1),
  (3, 1, 3),
  (2, 2, 1),
  (1, 1, 2) ]

instructionsStr :: [String]
instructionsStr = [
  "move 1 from 2 to 1",
  "move 3 from 1 to 3",
  "move 2 from 2 to 1",
  "move 1 from 1 to 2" ]

expectedCrates :: Array Int [Char]
expectedCrates = array (1, 3) [
  (1, ['C']),
  (2, ['M']),
  (3, ['Z', 'N', 'D', 'P']) ]

spec :: Spec
spec = do
  describe "permuteCrate" $ do
    it "move 3 crates from left to right" $ do
      permuteCrate Inverted 3 ['A', 'B', 'C', 'D', 'E'] ['F'] `shouldBe` (['D', 'E'], ['C', 'B', 'A', 'F'])
  describe "permuteCrates" $ do
    it "move crates as in sample data" $ do
      applyInstructions Inverted instructions crates `shouldBe` expectedCrates
  describe "readInstruction" $ do
    it "read the instruction successfully" $ do
      readInstruction "move 3 from 1 to 6" `shouldBe` (3, 1, 6)
  describe "day5Part1" $ do
    it "should return CMZ" $ do
      day5 Inverted instructionsStr crates `shouldBe` "CMZ"
  describe "day5Part2" $ do
    it "should return MCD" $ do
      day5 Same instructionsStr crates `shouldBe` "MCD"

