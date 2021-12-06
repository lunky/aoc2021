module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "Day6" $ do
    it "should do sample 1" $ do
      let input = "3,4,3,1,2"
      let expected=5934
      day6 input `shouldBe` expected
  describe "Day6b" $ do
    it "should do sample 1" $ do
      let days = 80
      let input = "3,4,3,1,2"
      let expected=5934
      day6b days input `shouldBe` expected
    it "should do sample 2" $ do
      let days = 256
      let input = "3,4,3,1,2"
      let expected=26984457539
      day6b days input `shouldBe` expected
