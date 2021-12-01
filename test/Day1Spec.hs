module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
    describe "day1" $
        it "should run test 1" $ do
          let input = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"
          let expected = 7
          day1 input `shouldBe` expected
    describe "day1b" $ 
        it "should run test 1b" $ do
          let input = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"
          let expected = 5
          day1b input `shouldBe` expected
