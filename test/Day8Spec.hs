module Day8Spec (spec) where

import Test.Hspec

import GHC.Arr

import Prelude hiding (Left, Right)
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

  describe "isVisible" $ do
    it "all boundary trees are visible" $ do
      [isVisible heightMap (1, j) | j <- [1..5]] `shouldBe` replicate 5 True
      [isVisible heightMap (5, j) | j <- [1..5]] `shouldBe` replicate 5 True
      [isVisible heightMap (i, 1) | i <- [1..5]] `shouldBe` replicate 5 True
      [isVisible heightMap (i, 5) | i <- [1..5]] `shouldBe` replicate 5 True
    it "some sample visibility test" $ do
      visibleFrom heightMap (2, 2) `shouldBe` [Up, Left]
      visibleFrom heightMap (2, 3) `shouldBe` [Up, Right]
      visibleFrom heightMap (2, 4) `shouldBe` []
      visibleFrom heightMap (3, 2) `shouldBe` [Right]
      visibleFrom heightMap (3, 3) `shouldBe` []
      visibleFrom heightMap (3, 4) `shouldBe` [Right]
      visibleFrom heightMap (4, 2) `shouldBe` []
      visibleFrom heightMap (4, 3) `shouldBe` [Left, Down]
      visibleFrom heightMap (4, 4) `shouldBe` []
    it "nbVisible trees" $ do
      day8part1 heightMapStr `shouldBe` 21



