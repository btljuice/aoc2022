
module Day12Spec(spec) where

import Test.Hspec
import Day12

sampleHmStr = [
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi" ]


spec :: Spec
spec = do
  describe "day12part1" $ do
    it "should find path from start to finish in 31 steps" $ do
      day12part1 sampleHmStr `shouldBe` 31
  describe "day12part2" $ do
    it "should find path from any a to finish in 29 steps" $ do
      day12part2 sampleHmStr `shouldBe` 29

