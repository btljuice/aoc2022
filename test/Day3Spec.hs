module Day3Spec (spec) where

import Test.Hspec
import Prelude
import Day3

sampleDataString = [
  "vJrwpWtwJgWrhcsFMMfFFhFp",
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
  "PmmdzqPrVvPwwTWBwg",
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
  "ttgJtRGJQctTZtZT",
  "CrZsJsPPZsGzwwsLwLmpwMDw" ]

spec :: Spec
spec = do
  describe "rucksacks only duplicated item priority" $ do
    it "sums to 157" $ do
      day3Part1 sampleDataString `shouldBe` 157

  describe "rucksacks only common item priority" $ do
    it "sums to 70" $ do
      day3Part2 sampleDataString `shouldBe` 70


