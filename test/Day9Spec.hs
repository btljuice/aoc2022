module Day9Spec (spec) where

import Test.Hspec

import Data.Set
import qualified Data.Set as Set

import Day9

motionsStr = [
  "R 4",
  "U 4",
  "L 3",
  "D 1",
  "R 4",
  "D 1",
  "L 5",
  "R 2" ]

-- ..##..
-- ...##.
-- .####.
-- ....#.
-- s###..

expectedVisitedByTail = Set.fromList [
  (0, 0), (1, 0), (2, 0), (3, 0),
  (4, 1), (4, 2), (4, 3), (1, 2), (3, 2),
  (3, 4), (2, 4), (3, 3), (2, 2) ]

spec :: Spec
spec = do
  describe "visitedByTail" $ do
    it "should visit the expected positionss" $ do
      day9part1 motionsStr `shouldBe` expectedVisitedByTail




