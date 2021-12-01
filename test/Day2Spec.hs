module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day2 input `shouldBe` expected
  describe "Day2b" $ do
    xit "should do sample 1" $ do
      let input=""
      let expected=0
      day2b input `shouldBe` expected
