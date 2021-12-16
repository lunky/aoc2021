module Day9Spec where

import Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "Day9" $ 
    it "should do sample 1" $ do
      let input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
      let expected=15
      day9 input `shouldBe` expected
  describe "Day9b" $ 
    it "should do sample 1" $ do
      let input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
      let expected=1134
      day9b input `shouldBe` expected
