module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  -- describe "foldy" $ do
  --   it "should do sample 1" $ do
  --     let input = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
  --     let expected=17
  --     day13 input `shouldBe` expected
  describe "Day13" $ do
    it "should do sample 1" $ do
      let input = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
      let expected=17
      day13 input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "0,0\n0,14\n\nfold along y=7"
      let expected=1
      day13 input `shouldBe` expected
    it "should do sample 3" $ do
      let input = "0,0\n14,0\n\nfold along x=7"
      let expected=1
      day13 input `shouldBe` expected
    it "should do sample 3a" $ do
      let input = "0,0\n14,0\n\nfold along x=2"
      let expected=2
      day13 input `shouldBe` expected
    it "should do sample 3b" $ do
      let input = "0,0\n14,0\n\nfold along x=8"
      let expected=2
      day13 input `shouldBe` expected
  -- describe "Day13b" $ do
  --   xit "should do sample 1" $ do
  --     let input=""
  --     let expected=0
  --     day13b input `shouldBe` expected
