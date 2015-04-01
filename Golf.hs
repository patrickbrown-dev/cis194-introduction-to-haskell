module Golf where

-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should contain
-- every second element from the input list... and the nth list in the output
-- should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips [] = [[]]
skips (x:_) = [[x]]

_skips :: Int -> [a] -> [a]
