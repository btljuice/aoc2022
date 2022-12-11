module LibSpec (spec) where

import Test.Hspec
-- import Test.Hspec.QuickCheck
import Lib (replace)

spec :: Spec
spec = do
  describe "replace" $ do
    it "should replace successfully in this trivial example" $
      replace [(0, '0'), (3, '3')] "abcdefgh" `shouldBe` "0bc3efgh"