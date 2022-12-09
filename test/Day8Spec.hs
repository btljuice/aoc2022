module Day8Spec (spec) where

import Test.Hspec
import Prelude

import GHC.Arr
import Day8

heightMapStr :: [String]
heightMapStr = [
  "30373",
  "25512",
  "65332",
  "33549",
  "35390" ]

heightMap = listArray ((1, 1), (5, 5)) [
  3, 0, 3, 7, 3,
  2, 5, 5, 1, 2,
  6, 5, 3, 3, 2,
  3, 3, 5, 4, 9,
  3, 5, 3, 9, 0 ]

spec :: Spec
spec = do
  describe "readHeightMap" $ do
    it "converts string heightMap into int 2-dim array" $ do
      readHeightMap heightMapStr `shouldBe` heightMap