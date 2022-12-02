module Day1Spec (spec) where

import Test.Hspec
import Day1

sampleData :: [[Integer]]
sampleData = [
  [1, 2, 3],
  [4],
  [5, 6],
  [7, 8, 9],
  [10] ]

spec :: Spec
spec = do
  describe "maxCaloriesPerElf" $ do
    it "returns the maximum calories carried (24) over sample data" $ do
      maxCaloriesPerElf sampleData `shouldBe` 24
