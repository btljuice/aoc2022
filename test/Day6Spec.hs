module Day6Spec (spec) where

import Test.Hspec
import Prelude

import Day6

subroutines = [
   "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
   "bvwbjplbgvbhsrlpgdmjqwftvncz",
   "nppdvjthqldpwncqszvftbrmjlhg",
   "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
   "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" ]

firstMarkers = [ 7, 5, 6, 10 ,11 ]

spec :: Spec
spec = do
  describe "sliding" $ do
    it "return sub windows of 2" $ do
      sliding 2 firstMarkers `shouldBe` [[7, 5], [5, 6], [6, 10], [10, 11]]
    it "return sub windows of 3" $ do
      sliding 3 firstMarkers `shouldBe` [[7, 5, 6], [5, 6, 10], [6, 10, 11]]
  describe "firstMarkerPos" $ do
    it "finds all first markers for subroutines" $ do
      map (firstMarkerPos 4) subroutines `shouldBe` firstMarkers

