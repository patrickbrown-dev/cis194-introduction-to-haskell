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
    it "should return the same tree if log type is unknown" $ do
      let tree = Leaf
          logMessage = Unknown "unknown"
      insert logMessage tree `shouldBe` tree
    it "should make a MessageTree data type on a Leaf tree" $ do
      let tree = Leaf
          logMessage = LogMessage (Error 1) 47 "Error 1 at 47"
      insert logMessage tree `shouldBe` Node Leaf logMessage Leaf
    it "should insert to the left side if timestamp is lessor" $ do
      let treeLog = LogMessage Info 50 "Info at 50"
          newLog = LogMessage Info 49 "Info at 49"
          tree = Node Leaf treeLog Leaf
      insert newLog tree `shouldBe` Node (Node Leaf newLog Leaf) treeLog Leaf
    it "should insert to the right side if timestamp is greater" $ do
      let treeLog = LogMessage Info 50 "Info at 50"
          newLog = LogMessage Info 51 "Info at 51"
          tree = Node Leaf treeLog Leaf
      insert newLog tree `shouldBe` Node Leaf treeLog (Node Leaf newLog Leaf)
    it "should insert middle value correctly in two node tree" $ do
      let rootLog = LogMessage Info 50 "root:50"
          leftLog = LogMessage Info 48 "left:48"
          newLog  = LogMessage Info 49 "new:49"
          tree    = Node (Node Leaf leftLog Leaf) rootLog Leaf
      insert newLog tree `shouldBe` Node (Node Leaf leftLog (Node Leaf newLog Leaf)) rootLog Leaf
