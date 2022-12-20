module Day16Spec(spec) where

import Test.Hspec
import Day16

valveStrs = [
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
  "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
  "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
  "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
  "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
  "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
  "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
  "Valve HH has flow rate=22; tunnel leads to valve GG",
  "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
  "Valve JJ has flow rate=21; tunnel leads to valve II" ]

valves = fmap readValve valveStrs


valveShortestPaths :: [(Label, Label, Int)]
valveShortestPaths =
  fmap (\ (from, to, l) -> (mkLabel from, mkLabel to, l))
  [("AA","BB",2),("AA","CC",3),("AA","DD",2),("AA","EE",3),("AA","HH",6),("AA","JJ",3),("BB","AA",2),("BB","CC",2),("BB","DD",3),("BB","EE",4),("BB","HH",7),("BB","JJ",4),("CC","AA",3),("CC","BB",2),("CC","DD",2),("CC","EE",3),("CC","HH",6),("CC","JJ",5),("DD","AA",2),("DD","BB",3),("DD","CC",2),("DD","EE",2),("DD","HH",5),("DD","JJ",4),("EE","AA",3),("EE","BB",4),("EE","CC",3),("EE","DD",2),("EE","HH",4),("EE","JJ",5),("HH","AA",6),("HH","BB",7),("HH","CC",6),("HH","DD",5),("HH","EE",4),("HH","JJ",8),("JJ","AA",3),("JJ","BB",4),("JJ","CC",5),("JJ","DD",4),("JJ","EE",5),("JJ","HH",8)]

spec :: Spec
spec = do
  describe "readValve" $ do
    it "should read valveStrs correctly" $ do
      head valves `shouldBe` mkValve "AA" 0 ["DD", "II", "BB"]
      last valves `shouldBe` mkValve "JJ" 21 ["II"]

  describe "shortestPaths" $ do
    it "should provide shortest paths for > 0 valves and start label" $ do
      (fmap (\p -> (head p, last p, length p)) . shortestPaths $ valves) `shouldBe` valveShortestPaths
