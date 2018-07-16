{-
驚くべきことに, 各桁を4乗した数の和が元の数と一致する数は3つしかない.

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

ただし, 1=1^4は含まないものとする. この数たちの和は 1634 + 8208 + 9474 = 19316 である.

各桁を5乗した数の和が元の数と一致するような数の総和を求めよ.
-}

module Lib
    ( someFunc
    ) where

import Data.Char (digitToInt)
        
someFunc :: IO ()
someFunc = print $ problem_30

limit :: Integer
limit = snd $ head $ dropWhile (\(a,b) -> a > b) $ zip (map (9^5*) [1..]) (map (10^) [1..])

fifth :: Integer -> Integer
fifth = sum . map ((^5) . toInteger . digitToInt) . show

problem_30 :: Integer
problem_30 = sum $ filter (\n -> n == fifth n) [2..limit]

main = do
    print $ problem_30
