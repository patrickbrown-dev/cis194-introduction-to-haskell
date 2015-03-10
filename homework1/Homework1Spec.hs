module Homework1Spec where

import Test.Hspec
import Homework1

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do
    it "turns an integer to a list of it's digits" $ do
      toDigits 1234 `shouldBe` [1, 2, 3, 4]
  describe "toDigitsRev" $ do
    it "turns an integer to a list of it's digits reversed" $ do
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
  describe "doubleEveryOther" $ do
    it "doubles every other digit in a list" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    it "starts with the secord to last digit" $ do
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
  describe "sumDigits" $ do
    it "sums the contents of a list" $ do
      sumDigits [16,7,12,5] `shouldBe` 1 + 6 + 7 + 1 + 2 + 5
  describe "validate" $ do
    it "returns false when card is invalid" $ do
      validate 4012888888881882 `shouldBe` False
    it "returns true when card is valid" $ do
      validate 4012888888881881 `shouldBe` True
  describe "hanoi" $ do
    it "describes the moves to complete a game with one disc" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "c")]
    it "describes the moves to complete a game with 2 discs" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
