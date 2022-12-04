module Day2Spec (spec) where

import Test.Hspec
import Prelude
import Day2

sampleDataString = unlines [
 "A Y",
 "B X",
 "C Z" ]


sampleData = [
  (R, P),
  (P, R),
  (S, S) ]

spec :: Spec
spec = do
  describe "setPoints" $ do
    it "returns correct number of point on sampleData" $ do
      setPoints sampleData `shouldBe` 15

