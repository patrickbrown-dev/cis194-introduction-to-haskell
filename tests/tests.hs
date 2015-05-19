module Main where

import Test.Hspec
import Homework1
import Log
import LogAnalysis
import Golf

main :: IO()
main = hspec $ do
  describe "Homework1" $ do
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

  describe "LogAnalysis" $ do
    describe "parseMessage" $ do
      it "should parse error messages from log" $ do
        parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      it "should parse info messages from log" $ do
        parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      it "should handle invalid logs" $ do
        parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
    describe "insert" $ do
      it"should return the same tree if log type is unknown" $ do
        let tree   = Leaf
            badLog = Unknown "unknown"
        insert badLog tree `shouldBe` tree
      it "should make a MessageTree data type on a Leaf tree" $ do
        let tree   = Leaf
            newLog = LogMessage (Error 1) 47 "Error 1 at 47"
        insert newLog tree `shouldBe` Node Leaf newLog Leaf
      it "should insert to the left side if timestamp is lessor" $ do
        let treeLog = LogMessage Info 50 "Info at 50"
            newLog  = LogMessage Info 49 "Info at 49"
            tree    = Node Leaf treeLog Leaf
        insert newLog tree `shouldBe` Node (Node Leaf newLog Leaf) treeLog Leaf
      it "should insert to the right side if timestamp is greater" $ do
        let treeLog = LogMessage Info 50 "Info at 50"
            newLog  = LogMessage Info 51 "Info at 51"
            tree    = Node Leaf treeLog Leaf
        insert newLog tree `shouldBe` Node Leaf treeLog (Node Leaf newLog Leaf)
      it "should insert middle value correctly in two node tree" $ do
        let rootLog = LogMessage Info 50 "root:50"
            leftLog = LogMessage Info 48 "left:48"
            newLog  = LogMessage Info 49 "new:49"
            tree    = Node (Node Leaf leftLog Leaf) rootLog Leaf
        insert newLog tree `shouldBe` Node (Node Leaf leftLog (Node Leaf newLog Leaf)) rootLog Leaf
    describe "build" $ do
      it "should built a correct tree" $ do
        let rootLog  = LogMessage Info 50 "root:50"
            leftLog  = LogMessage Info 49 "left:49"
            rightLog = LogMessage Info 51 "right:51"
        build [rootLog, leftLog, rightLog] `shouldBe` Node (Node Leaf leftLog Leaf) rootLog (Node Leaf rightLog Leaf)
    describe "inOrder" $ do
      it "should return logs in order" $ do
        let firstLog  = LogMessage Info 51 "1st:51"
            secondLog = LogMessage Info 52 "2nd:52"
            thirdLog  = LogMessage Info 53 "3rd:53"
            fourthLog = LogMessage Info 54 "4th:54"
            tree      = build [fourthLog, secondLog, firstLog, thirdLog]
        inOrder tree `shouldBe` [firstLog, secondLog, thirdLog, fourthLog]
      it "should parse out Unknown messages" $ do
        let firstLog   = LogMessage Info 51 "1st:51"
            secondLog  = LogMessage Info 52 "2nd:52"
            thirdLog   = LogMessage Info 53 "3rd:53"
            unknownLog = Unknown "unknown"
            tree       = build [secondLog, unknownLog, firstLog, thirdLog]
        inOrder tree `shouldBe` [firstLog, secondLog, thirdLog]
    describe "whatWentWrong" $ do
      it "should filter out error messages with severity above 49" $ do
        let log1 = LogMessage Info 51 "duck"
            log2 = LogMessage (Error 49) 53 "duck"
            log3 = Unknown "duck"
            log4 = LogMessage (Error 50) 52 "goose"
            list = [log1, log2, log3, log4]
        whatWentWrong list `shouldBe` ["goose"]

  describe "Golf" $ do
    describe "skips" $ do
      it "should contain every n element from the input list in the nth list" $ do
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    describe "lastOfGroup" $ do
      it "should group into sublist modulo n" $ do
        lastOfGroup 3 [1,2,3,4,5,6,7,8,9] `shouldBe` [3,6,9]
      it "should ignore incomplete groups" $ do
        lastOfGroup 3 [1,2,3,4,5,6,7,8] `shouldBe` [3,6]
