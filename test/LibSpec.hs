module LibSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Lib (palindrome)

spec :: Spec
spec = do
  describe "palindrome" $ do
    prop "reverses a string" $
      \x -> palindrome (x :: String) `shouldBe` palindrome (reverse x)