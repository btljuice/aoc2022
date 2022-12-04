module Day1Spec (spec) where

import Test.Hspec
import Prelude
import Day1
sampleDataString = "1\n2\n3\n\n4\n\n5\n6\n\n7\n8\n9\n\n10"

sampleData = [
  [1, 2, 3],
  [4],
  [5, 6],
  [7, 8, 9],
  [10] ]

spec :: Spec
spec = do
  describe "maxCalories" $ do
    it "returns the maximum calories carried (24) over sample data" $ do
      (maxCalories . caloriesPerElf) sampleData `shouldBe` 24
  describe "readCalories" $ do
    it "converts `sampleDataString` to `sampleData`" $ do
      (readCalories . lines)  sampleDataString `shouldBe` sampleData
  describe "select top 3" $ do
    it "returns top three elves in `sampleData`" $ do
      (topCalories 3 . caloriesPerElf) sampleData `shouldBe` [24, 11, 10]

