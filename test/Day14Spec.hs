module Day14Spec(spec) where

import Test.Hspec
import Day14

samplePaths :: [String]
samplePaths = [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9" ]

(pouredArr, nbSand, isAbyss) = day14part1 samplePaths

spec :: Spec
spec = do
  describe "day14part1" $ do
    it "should solution correctly" $ do
      nbSand `shouldBe` 24
      isAbyss `shouldBe` True

