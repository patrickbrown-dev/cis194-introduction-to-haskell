module LogAnalysisSpec where

import Test.Hspec
import Log
import LogAnalysis

main :: IO ()
main = hspec $ do
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
