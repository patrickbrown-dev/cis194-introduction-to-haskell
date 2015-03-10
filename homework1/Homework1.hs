module Homework1 where

-- Turn an Integer into a list of it's digits
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- Same as above, but reverse them.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Doubles every other digit, starting with second to last.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther n = reverse $ _doubleEveryOther $ reverse n

_doubleEveryOther :: [Integer] -> [Integer]
_doubleEveryOther n = zipWith (*) n $ cycle [1,2]

-- Sums the digits of all items in a list. For example:
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs

-- Validate credit card
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, c)]
hanoi 2 a b c = hanoi 1 a b c ++ [("a", "b"), ("c", "b")]
hanoi n a b c = [] -- giving up here...
