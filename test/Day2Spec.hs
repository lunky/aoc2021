module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ 
    it "should do sample 1" $ do
      let input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
      let expected=150
      day2 input `shouldBe` expected
  describe "Day2b" $ do
    it "should do sample 1" $ do
      let input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
      let expected=900
      day2b input `shouldBe` expected
    describe "move" $ do
      it "should follow basic command down" $ 
        move ("down", 4) (1,1) `shouldBe` (1,5)
      it "should follow basic command up" $ 
        move ("up", 4) (1,1) `shouldBe` (1,-3)
      it "should follow basic command forward" $ 
        move ("forward", 4) (1,1) `shouldBe` (5,1)
    describe "move2" $ do
      it "should follow basic command forward" $ 
        move2 ("forward", 5) (0,0,0) `shouldBe` (5,0,0)
      it "should follow basic command down" $ 
        move2 ("down", 5) (5,0,0) `shouldBe` (5,0,5)
      it "should follow basic command forward" $ 
        move2 ("forward", 8) (5,0,5) `shouldBe` (13,40,5)
      it "up 3 decreases your aim by 3, resulting in a value of 2." $ 
        move2 ("up", 3) (13, 40, 5) `shouldBe` (13, 40, 2)
      it "down 8 adds 8 to your aim, resulting in a value of 10." $ 
        move2 ("down", 8) (13, 40, 2) `shouldBe` (13, 40, 10)
      it "forward 2 adds 2 to your horizontal position, a total of 15." $ 
        move2 ("forward", 2) (13, 40, 10) `shouldBe` (15, 60, 10)


