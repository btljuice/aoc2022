
module Day12Spec(spec) where

import Test.Hspec
import Day12
import Data.Array ((!))

sampleHmStr = [
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi" ]

hm = readHeightMap sampleHmStr
start = (1, 1)
end = (3, 6)

spec :: Spec
spec = do
  describe "day12part1" $ do
    it "should find path from start to finish in 31 steps" $ do
      fmap snd ( findPath hm start end ! (1, 1) ) `shouldBe` Just 31

