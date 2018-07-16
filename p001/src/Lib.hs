{-
1以上、1000未満の整数で、3または5で割り切れるものの和を求める
複数の解決法を提示し、評価せよ
-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
    print $ problem_1_1 1 999
    print $ problem_1_2 1 999
    print $ problem_1_3 1 999

problem_1_1 min max = sum $ [x | x <- [min..max], x `mod` 3 == 0 || x `mod` 5 == 0]
    
myfilter n = (n `mod` 3 == 0 || n `mod` 5 == 0)
problem_1_2 min max = sum $ [x | x <- [min..max], myfilter x]
    
problem_1_3 min max = sum $ filter(myfilter) [min..max]
    