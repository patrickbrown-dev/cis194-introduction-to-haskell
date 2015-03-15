{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
  | otherwise = Unknown n

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
