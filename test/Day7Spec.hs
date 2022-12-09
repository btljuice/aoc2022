
module Day7Spec (spec) where

import Test.Hspec
import Prelude

import Day7

commandStrings = [
  "$ ls",
  "dir a",
  "14848514 b.txt",
  "8504156 c.dat",
  "dir d",
  "$ cd a",
  "$ ls",
  "dir e",
  "29116 f",
  "2557 g",
  "62596 h.lst",
  "$ cd e",
  "$ ls",
  "584 i",
  "$ cd ..",
  "$ cd ..",
  "$ cd d",
  "$ ls",
  "4060174 j",
  "8033020 d.log",
  "5626152 d.ext",
  "7214296 k" ]

commandsParsed :: [Command]
commandsParsed = [
  Ls [
    Dir "a",
    File 14848514 "b.txt",
    File 8504156 "c.dat",
    Dir "d" ],
  CdDown "a",
  Ls [
    Dir "e",
    File 29116 "f",
    File 2557 "g",
    File 62596 "h.lst" ],
  CdDown "e",
  Ls [
    File 584 "i" ],
  CdUp,
  CdUp,
  CdDown "d",
  Ls [
    File 4060174 "j",
    File 8033020 "d.log",
    File 5626152 "d.ext",
    File 7214296 "k" ] ]


spec :: Spec
spec = do
  describe "readCommands" $ do
    it "converts commandsStr into commandsParsed" $ do
      readCommands commandStrings `shouldBe` commandsParsed

  describe "dirsWith" $ do
    it "dirsWith on sampled command should return 95437" $ do
      day1part1 commandStrings `shouldBe` 95437


