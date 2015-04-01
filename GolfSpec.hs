module GolfSpec where

import Test.Hspec
import Golf

main :: IO()
main = hspec $ do
  describe "skips" $ do
    it "should make the first list in the output the same as the input list" $ do
      let (x:_) = skips "ABCD"
      x `shouldBe` "ABCD"
    it "should contain every second element from the input list in the second list" $ do
      let (_:x:_) = skips "ABCD"
      x `shouldBe` "BD"
    it "should contain every n element from the input list in the nth list" $ do
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
