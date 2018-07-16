{-
2520 は 1 から 10 の数字の全ての整数で割り切れる数字であり, そのような数字の中では最小の値である.
では, 1 から 20 までの整数全てで割り切れる数字の中で最小の正の数はいくらになるか.
-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ problem_5

problem_5 = foldr1 lcm [1..20]
