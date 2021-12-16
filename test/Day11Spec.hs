module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ 
    it "should do sample 1" $ do
      let input = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
      let expected=1656
      day11 input `shouldBe` expected
  describe "Day11b" $ 
    it "should do sample 1" $ do
      let input = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"
      let expected=195
      day11b input `shouldBe` expected
