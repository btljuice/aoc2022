module Day4Spec (spec) where

import Test.Hspec
import Prelude
import Day4

sampleDataString = [
  "2-4,6-8",
  "2-3,4-5",
  "5-7,7-9",
  "2-8,3-7",
  "6-6,4-6",
  "2-6,4-8" ]

spec :: Spec
spec = do
  describe "day4" $ do
    it "day4part1 find 2 assigments pairs being a subset of the other" $ do
      day4Part1 sampleDataString `shouldBe` 2
    it "day4part2 find 4 assigments pairs overlapping" $ do
      day4Part2 sampleDataString `shouldBe` 4


