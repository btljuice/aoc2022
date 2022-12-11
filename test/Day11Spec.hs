module Day11Spec(spec) where

import Test.Hspec
import Day11
import Day11 (day11part1)

sampleMonkeys :: [Monkey]
sampleMonkeys = [
  Monkey {
    items = [79, 98],
    update = (* 19),
    throwTo = divisibleBy 23 2 3
  },
  Monkey {
    items = [54, 65, 75, 74],
    update = (+ 6),
    throwTo = divisibleBy 19 2 0
  },
  Monkey {
    items = [79, 60, 97],
    update = \ w -> w*w,
    throwTo = divisibleBy 13 1 3
  },
  Monkey {
    items = [74],
    update = (+ 3),
    throwTo = divisibleBy 17 0 1
  } ]
  where divisibleBy d t f w = if w `mod` d == 0 then t else f

itemsOnly :: [Monkey] -> [[WorryLevel]]
itemsOnly = map items

spec :: Spec
spec = do
  describe "20 rounds of monkeys" $ do
    it "have items at expected places" $ do
      (itemsOnly . fst . last  . rounds 20 $ sampleMonkeys) `shouldBe` [[10, 12, 14, 26, 34], [245, 93, 53, 199, 115], [], []]
  describe "day11part1" $ do
    it "should return 10605 on sample" $ do
      day11part1 sampleMonkeys `shouldBe` 10605