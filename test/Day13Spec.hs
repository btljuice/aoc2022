
module Day13Spec(spec) where

import Test.Hspec
import Day13

sampleStr = [
  "[1,1,3,1,1]",
  "[1,1,5,1,1]",
  "",
  "[[1],[2,3,4]]",
  "[[1],4]",
  "",
  "[9]",
  "[[8,7,6]]",
  "",
  "[[4,4],4,4]",
  "[[4,4],4,4,4]",
  "",
  "[7,7,7,7]",
  "[7,7,7]",
  "",
  "[]",
  "[3]",
  "",
  "[[[]]]",
  "[[]]",
  "",
  "[1,[2,[3,[4,[5,6,7]]]],8,9]",
  "[1,[2,[3,[4,[5,6,0]]]],8,9]" ]

spec :: Spec
spec = do
  describe "day13part1" $ do
    it "should sum to 13" $ do
      day13part1 sampleStr `shouldBe` 13
  describe "day13part2" $ do
    it "should times to 140" $ do
      day13part2 sampleStr `shouldBe` 140

