module Day15Spec(spec) where

import Test.Hspec
import Day15

sensorStrs = [
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3" ]

sensors = fmap readSensor sensorStrs

spec :: Spec
spec = do
  describe "day15part1" $ do
    it "should solution correctly" $ do
      day15part1 sensorStrs 10 `shouldBe` 26
  describe "merge" $ do
    it "should merge correctly following ranges" $ do
      merge (0, 1) (3, 4) `shouldBe` ((0, 1), Just (3, 4))  -- Don't touch
      merge (0, 1) (2, 4) `shouldBe` ((0, 4), Nothing)  -- touch
      merge (-3, 1) (0, 4) `shouldBe` ((-3, 4), Nothing) -- overlaps
      merge (-3, 5) (0, 4) `shouldBe` ((-3, 5), Nothing) -- contains
      --- Inverted position
      merge (3, 4) (0, 1)  `shouldBe` ((0, 1), Just (3, 4))  -- Don't touch
      merge (2, 4) (0, 1)  `shouldBe` ((0, 4), Nothing)  -- touch
      merge (0, 4) (-3, 1) `shouldBe` ((-3, 4), Nothing) -- overlaps
      merge (0, 4) (-3, 5) `shouldBe` ((-3, 5), Nothing) -- contains

  describe "holesInBound" $ do
    it "should find the right holes in the sample" $ do
      holesInBound 0 20 sensors 11 `shouldBe` [14]

  describe "day15part2" $ do
    it "should solution correctly" $ do
      day15part2 sensorStrs 0 20 `shouldBe` (56000011, (14, 11))
