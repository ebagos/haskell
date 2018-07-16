{-
n × (n - 1) × ... × 3 × 2 × 1 を n! と表す.

例えば, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800 となる.
この数の各桁の合計は 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27 である.

では, 100! の各桁の数字の和を求めよ.
-}
module Lib
( someFunc
) where

import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = print $ problem_20

factorial n = product [1..n]
nsum n = sum $ map digitToInt $ show n

problem_20 = nsum $ factorial 100
