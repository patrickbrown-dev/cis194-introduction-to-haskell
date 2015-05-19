module Golf where

-- The output of skips is a list of lists. The first list in the output
-- should be the same as the input list. The second list in the output
-- should contain every second element from the input list... and the
-- nth list in the output should contain every nth element from the
-- input list.
skips :: [a] -> [[a]]
skips xs = [xs]

-- skips' :: Int -> Int -> [a] -> [[a]]
-- skips' _ _ [] = [[]]
-- skips' n len arr
--   | n < len = [skipN arr n] ++ (skips' (n + 1) len arr)
--   | otherwise = [[]]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n > 0 = (take n xs) : (group n (drop n xs))
  | otherwise = error "Negative n"



-- _skips _ _ [] = []
-- _skips n x (y:ys)
--   | x `mod` n == 0 = [y] ++ (_skips n (x + 1) ys)
--   | otherwise      = _skips n (x + 1) ys
