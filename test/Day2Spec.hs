module Day2Spec (spec) where

import Test.Hspec
import Prelude
import Day2

sampleDataString = [
 "A Y",
 "B X",
 "C Z" ]


sampleData = [
  (R, P),
  (P, R),
  (S, S) ]

sampleData2 = [
  (R, D),
  (P, L),
  (S, W) ]

spec :: Spec
spec = do
  describe "readBout" $ do
    it "converts sampleDataString to sampleData" $ do
      map readBout sampleDataString `shouldBe` sampleData
  describe "gamePoints" $ do
    it "returns correct number of point on sampleData" $ do
      gamePoints sampleData `shouldBe` 15


  describe "readBout2" $ do
    it "converts sampleDataString to sampleData2" $ do
      map readBout2 sampleDataString `shouldBe` sampleData2
  describe "gamePoints2" $ do
    it "returns correct number of point on sampleData2" $ do
      gamePoints2 sampleData2 `shouldBe` 12


