module Day11Spec(spec) where

import Test.Hspec
import Day11
import Day11 (Monkey(shouldRelief), nbThrownItems)

sampleMonkeys :: [Monkey]
sampleMonkeys = [
  Monkey {
    items = [79, 98],
    update = (* 19),
    throwTo = divisibleBy 23 2 3,
    shouldRelief = True
  },
  Monkey {
    items = [54, 65, 75, 74],
    update = (+ 6),
    throwTo = divisibleBy 19 2 0,
    shouldRelief = True
  },
  Monkey {
    items = [79, 60, 97],
    update = \ w -> w*w,
    throwTo = divisibleBy 13 1 3,
    shouldRelief = True
  },
  Monkey {
    items = [74],
    update = (+ 3),
    throwTo = divisibleBy 17 0 1,
    shouldRelief = True
  } ]
  where divisibleBy d t f w = if w `mod` d == 0 then t else f

itemsOnly :: [Monkey] -> [[WorryLevel]]
itemsOnly = map items

noRelief :: [Monkey] -> [Monkey]
noRelief = map (\m -> m{shouldRelief = False})

spec :: Spec
spec = do
  describe "20 rounds of monkeys" $ do
    it "have items at expected places" $ do
      (itemsOnly . fst . last  . rounds 20 $ sampleMonkeys) `shouldBe` [[10, 12, 14, 26, 34], [245, 93, 53, 199, 115], [], []]
  describe "day11part1" $ do
    it "should return 10605 on sample" $ do
      day11part1 sampleMonkeys `shouldBe` 10605
  describe "nbThrownItems" $ do
    it "after 20 rounds" $ do
      nbThrownItems 20 sampleMonkeys `shouldBe` [101, 95, 7, 105]
    it "no relief - after 1 rounds" $ do
      nbThrownItems 1 (noRelief sampleMonkeys) `shouldBe` [2, 4, 3, 6]
    it "no relief - after 20 rounds" $ do
      nbThrownItems 20 (noRelief sampleMonkeys) `shouldBe` [99, 97, 8, 103]
    it "no relief - after 1000 rounds" $ do
    --   nbThrownItems 1000 (noRelief sampleMonkeys) `shouldBe` [5204, 4792, 199, 5192]
    -- it "no relief - after 2000 rounds" $ do
    --   nbThrownItems 2000 (noRelief sampleMonkeys) `shouldBe` [10419, 9577, 392, 10391]