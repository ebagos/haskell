{-
2^15 = 32768 であり, 各位の数字の和は 3 + 2 + 7 + 6 + 8 = 26 となる.

同様にして, 2^1000 の各位の数字の和を求めよ.
-}
module Lib
    ( someFunc
    ) where

import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = print $ problem_16

nsum n = sum $ map digitToInt $ show n
problem_16 = nsum (2 ^ 1000)
