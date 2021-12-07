module Day8Spec where

import Day8
import Test.Hspec

spec :: Spec
spec = do
  describe "Day8" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day8 input `shouldBe` expected
  describe "Day8b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day8b input `shouldBe` expected
