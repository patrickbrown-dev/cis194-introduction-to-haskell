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
