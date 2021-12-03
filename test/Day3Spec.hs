module Day3Spec where

import Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "Day3" $ do
    it "should do sample 1" $ do
      let input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
      let expected=198
      day3 input `shouldBe` expected
  describe "Day3b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day3b input `shouldBe` expected
