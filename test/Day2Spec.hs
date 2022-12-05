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

spec :: Spec
spec = do
  describe "readBout" $ do
    it "converts sampleDataString to sampleData" $ do
      map readBout sampleDataString `shouldBe` sampleData
  describe "gamePoints" $ do
    it "returns correct number of point on sampleData" $ do
      gamePoints sampleData `shouldBe` 15

  describe "readRPS" $ do
    it "returns R" $ do
      readRPS "A" `shouldBe` R
      readRPS "X" `shouldBe` R
    it "returns P" $ do
      readRPS "B" `shouldBe` P
      readRPS "Y" `shouldBe` P
    it "returns S" $ do
      readRPS "C" `shouldBe` S
      readRPS "Z" `shouldBe` S

