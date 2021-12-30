module Day12Spec where

import Day12
import Test.Hspec

spec :: Spec
spec = do
  describe "Day12" $ do
    it "should do sample 1" $ do
      let input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
      let expected=10
      day12 input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
      let expected=19
      day12 input `shouldBe` expected
    it "should do sample 3" $ do
      let input = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"
      let expected=226
      day12 input `shouldBe` expected
  describe "Day12b" $ do
    it "should do sample 1" $ do
      let input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
      let expected=36
      day12b input `shouldBe` expected
    it "should do sample 2" $ do
      let input = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"
      let expected=103
      day12b input `shouldBe` expected
    it "should do sample 3" $ do
      let input = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"
      let expected= 3509
      day12b input `shouldBe` expected
