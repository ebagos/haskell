{-
左右どちらから読んでも同じ値になる数を回文数という.
2桁の数の積で表される回文数のうち, 最大のものは 9009 = 91 × 99 である.
では, 3桁の数の積で表される回文数の最大値を求めよ.
-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ problem_4

problem_4 = 
    maximum [x | y<-[100..999], z<-[100..999], 
        let x = y * z,
        let s = show x,
        s == reverse s]
