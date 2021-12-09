module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
  describe "Day7" $ 
    it "should do sample 1" $ do
      let input = "16,1,2,0,4,2,7,1,2,14"
      let expected=37
      day7 input `shouldBe` expected
  describe "Day7b" $ 
    it "should do sample 1" $ do
      let input = "16,1,2,0,4,2,7,1,2,14"
      let expected=168
      day7b input `shouldBe` expected
