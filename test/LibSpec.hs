module LibSpec (spec) where

import Test.Hspec
-- import Test.Hspec.QuickCheck
import Lib (replace, shortestPath)

edgesUnparsed = [
  ("AA", ["DD", "II", "BB"]),
  ("BB", ["CC", "AA"]),
  ("CC", ["DD", "BB"]),
  ("DD", ["CC", "AA", "EE"]),
  ("EE", ["FF", "DD"]),
  ("FF", ["EE", "GG"]),
  ("GG", ["FF", "HH"]),
  ("HH", ["GG"]      ),
  ("II", ["AA", "JJ"]),
  ("JJ", ["II"]      ) ]

edges = do
  (from, tos) <- edgesUnparsed
  to <- tos
  [(from, to), (to, from)]

spec :: Spec
spec = do
  describe "replace" $ do
    it "should replace successfully in this trivial example" $
      replace [(0, '0'), (3, '3')] "abcdefgh" `shouldBe` "0bc3efgh"

  describe "shortestPath" $ do
    it "should find shortest paths for all symbols" $ do
      shortestPath edges "AA" "ZZ" `shouldBe` []
      shortestPath edges "AA" "AA" `shouldBe` ["AA"]
      shortestPath edges "AA" "DD" `shouldBe` ["AA", "DD"]
      shortestPath edges "AA" "CC" `shouldBe` ["AA", "DD", "CC"]
      shortestPath edges "AA" "EE" `shouldBe` ["AA", "DD", "EE"]
      shortestPath edges "AA" "FF" `shouldBe` ["AA", "DD", "EE", "FF"]
      shortestPath edges "AA" "GG" `shouldBe` ["AA", "DD", "EE", "FF", "GG"]
      shortestPath edges "AA" "HH" `shouldBe` ["AA", "DD", "EE", "FF", "GG", "HH"]
      shortestPath edges "AA" "II" `shouldBe` ["AA", "II"]
      shortestPath edges "AA" "JJ" `shouldBe` ["AA", "II", "JJ"]
      shortestPath edges "AA" "BB" `shouldBe` ["AA", "BB"]
