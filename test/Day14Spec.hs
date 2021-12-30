module Day14Spec where

import Day14
import Test.Hspec
import Data.Map (Map)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Day14'" $ do 
    it "should do sample 1" $ do
      let input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
      let expected = pairs "NCNBCHB"
      day14' 1 input `shouldBe` expected

    it "should do sample 2" $ do
      let input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
      let expected = pairs "NBCCNBBBCBHCB"
      day14' 2 input `shouldBe` expected

    it "should do sample 3" $ do
      let input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
      let expected = pairs "NBBBCNCCNBBNBNBBCHBHHBCHB"
      day14' 3 input `shouldBe` expected

  describe "Day14" $ 
    it "should do sample 1" $ do
      let input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
      let expected=1588
      day14 input `shouldBe` expected

  describe "Day14b" $ 
    it "should do sample 1" $ do
      let input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
      let expected=2188189693529
      day14b input `shouldBe` expected
