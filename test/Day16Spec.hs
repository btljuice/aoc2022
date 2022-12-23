module Day16Spec(spec) where

import Test.Hspec
import Day16
import Data.Array((//), (!))
import Day16 (flowRates)

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

expectedTravelCosts = mkTravelCosts //
  fmap (\(from, to, cost) -> ((mkLabel from, mkLabel to), cost))
  [("AA","BB",1),
   ("AA","CC",2),
   ("AA","DD",1),
   ("AA","EE",2),
   ("AA","HH",5),
   ("AA","JJ",2),
   ("BB","AA",1),
   ("BB","CC",1),
   ("BB","DD",2),
   ("BB","EE",3),
   ("BB","HH",6),
   ("BB","JJ",3),
   ("CC","AA",2),
   ("CC","BB",1),
   ("CC","DD",1),
   ("CC","EE",2),
   ("CC","HH",5),
   ("CC","JJ",4),
   ("DD","AA",1),
   ("DD","BB",2),
   ("DD","CC",1),
   ("DD","EE",1),
   ("DD","HH",4),
   ("DD","JJ",3),
   ("EE","AA",2),
   ("EE","BB",3),
   ("EE","CC",2),
   ("EE","DD",1),
   ("EE","HH",3),
   ("EE","JJ",4),
   ("HH","AA",5),
   ("HH","BB",6),
   ("HH","CC",5),
   ("HH","DD",4),
   ("HH","EE",3),
   ("HH","JJ",7),
   ("JJ","AA",2),
   ("JJ","BB",3),
   ("JJ","CC",4),
   ("JJ","DD",3),
   ("JJ","EE",4),
   ("JJ","HH",7)]

expectedFlowRates = mkFlowRates // [
 (mkLabel "AA", 0),
 (mkLabel "BB", 13),
 (mkLabel "CC", 2),
 (mkLabel "DD", 20),
 (mkLabel "EE", 3),
 (mkLabel "FF", 0),
 (mkLabel "GG", 0),
 (mkLabel "HH", 22),
 (mkLabel "II", 0),
 (mkLabel "JJ", 21) ]

spec :: Spec
spec = do
  describe "readValve" $ do
    it "should read valveStrs correctly" $ do
      head valves `shouldBe` mkValve "AA" 0 ["DD", "II", "BB"]
      last valves `shouldBe` mkValve "JJ" 21 ["II"]

  describe "travelCosts" $ do
    it "should equal expectedTravelCosts" $ do
      travelCosts valves `shouldBe` expectedTravelCosts

  describe "flowRates" $ do
    it "should equal expected value" $ do
      flowRates valves`shouldBe` expectedFlowRates

