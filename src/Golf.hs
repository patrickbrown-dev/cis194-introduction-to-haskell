module Golf where

-- The output of skips is a list of lists. The first list in the output
-- should be the same as the input list. The second list in the output
-- should contain every second element from the input list... and the
-- nth list in the output should contain every nth element from the
-- input list.
skips :: [a] -> [[a]]
skips [] = [[]]
skips xs = skips' 1 (length xs) xs

skips' :: Int -> Int -> [a] -> [[a]]
skips' _ _ [] = [[]]
skips' n len xs = if n <= len
                  then lastOfGroup n xs : skips' (n + 1) len xs
                  else lastOfGroup n [xs] 

lastOfGroup :: Int -> [a] -> [a]
lastOfGroup _ [] = []
lastOfGroup n xs
  | n /= length x  = []
  | n > 0          = last x : lastOfGroup n (drop n xs)
  | otherwise      = error "Negative n"
  where x = take n xs


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | (x < y) && (y > z) = y : localMaxima ([y] ++ [z] ++ xs)
  | otherwise          = localMaxima ([y] ++ [z] ++ xs)
localMaxima _ = []
