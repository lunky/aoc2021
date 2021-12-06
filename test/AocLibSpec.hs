module AocLibSpec where
import AocLib
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "AocLib" $ do
    it "should demonstrate that AocLib is working" $ 
      doubleFunc 1 `shouldBe` 2
    it "should demonstrate that grid is working" $ do
      let input = [["a","b","c"],["d","e","f"],["g","h","i"]]
      let output = grid input
      let expected = length $ concat input
      length output `shouldBe` expected
    it "should demonstrate that grid is working 2" $ do
      let input = [["a","b","c"],["d","e","f"],["g","h","i"]]
      let output = grid input
      last output `shouldBe` ((3,3),"i")

