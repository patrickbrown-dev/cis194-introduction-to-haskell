{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

build :: [LogMessage] -> MessageTree
build [] = Leaf
build n = _build n Leaf

_build :: [LogMessage] -> MessageTree -> MessageTree
_build [] tree = tree
_build (x:xs) tree = let newTree = insert x tree
                     in _build xs newTree


-- A MessageTree should be sorted by timestamp: that is, the timestamp of a
-- LogMessage in any Node should be greater than all timestamps of any
-- LogMessage in the left subtree, and less than all timestamps of any
-- LogMessage in the right child.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) n = n
insert newLog Leaf = Node Leaf newLog Leaf
insert newLog (Node left nodeLog right)
  | newLog `isLessThanLog` nodeLog = Node (insert newLog left) nodeLog right
  | otherwise                      = Node left nodeLog (insert newLog right)

isLessThanLog :: LogMessage -> LogMessage -> Bool
isLessThanLog (LogMessage _ xtime _) (LogMessage _ ytime _) = xtime < ytime

parse :: String -> [LogMessage]
parse n =
  let logs = lines n
  in map parseMessage logs

-- Parse a log data type from a string.
parseMessage :: String -> LogMessage
parseMessage n
  | isError n   = parseError n
  | isWarning n = parseWarning n
  | isInfo n    = parseInfo n
  | otherwise   = Unknown n

parseError :: String -> LogMessage
parseError n =
  let (_:code:timestamp:message) = words n
  in LogMessage (Error (read code :: Int)) (read timestamp :: Int) (unwords message)

parseWarning :: String -> LogMessage
parseWarning n =
  let (_:timestamp:message) =  words n
  in LogMessage Warning (read timestamp :: Int) (unwords message)

parseInfo :: String -> LogMessage
parseInfo n =
  let (_:timestamp:message) = words n
  in LogMessage Info (read timestamp :: Int) (unwords message)

isError :: String -> Bool
isError (x:_) = if x == 'E'
                then True
                else False
isError _ = False

isWarning :: String -> Bool
isWarning (x:_) = if x == 'W'
                  then True
                  else False
isWarning _ = False


isInfo :: String -> Bool
isInfo (x:_) = if x == 'I'
               then True
               else False
isInfo _ = False
