module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  describe "Day15" $ do
    it "should do sample 1" $ do
      let expected=40
      day15 _input `shouldBe` expected
    it "should do sample 2" $ do
      let expected=619
      contents <- readFile "data/day15.txt"
      day15 contents `shouldBe` expected
  describe "Day15b" $ 
    it "should do sample 1" $ do
      let expected=315
      day15b _input `shouldBe` expected
