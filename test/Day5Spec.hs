module Day5Spec where

import Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "Day5" $ 
    it "should do sample 1" $ do
      let input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
      let expected=5
      day5 input `shouldBe` expected
  describe "Day5b" $ 
    it "should do sample 1" $ do
      let input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
      let expected=12
      day5b input `shouldBe` expected
  describe "plotLine" $ do
    it "An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3." $ do 
      let input = [(1,1),(3,3)]
      let expected=[(1,1),(2,2),(3,3)]
      plotLine input `shouldMatchList` expected
    it "An entry like  3,3 -> 1,1 covers points 1,1, 2,2, and 3,3." $ do 
      let input = [(3,3),(1,1)]
      let expected=[(1,1),(2,2),(3,3)]
      plotLine input `shouldMatchList` expected
    it "An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9." $ do 
      let input = [(9,7),(7,9)]
      let expected=[(9,7),(8,8),(7,9)]
      plotLine input `shouldMatchList` expected
    it "An entry like 7,9 -> 9,7 covers points 9,7, 8,8, and 7,9." $ do 
      let input = [(7,9),(9,7)]
      let expected=[(9,7),(8,8),(7,9)]
      plotLine input `shouldMatchList` expected

