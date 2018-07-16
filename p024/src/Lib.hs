{-
順列とはモノの順番付きの並びのことである. たとえば, 3124は数 1, 2, 3, 4 の一つの順列である. すべての順列を数の大小でまたは辞書式に並べたものを辞書順と呼ぶ. 0と1と2の順列を辞書順に並べると
012 021 102 120 201 210
になる.

0,1,2,3,4,5,6,7,8,9からなる順列を辞書式に並べたときの100万番目はいくつか?
-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print problem_24

-- 順列の総数
permutationSize :: Int -> Int
permutationSize n = product [2 .. n]
 
-- xs の順列の n 番目の要素を返す。(0 番目から数え始める)
permutationCount :: [a] -> Int -> [a]
permutationCount xs 0 = xs
permutationCount xs n = b : permutationCount (as ++ bs) r
    where
      (q, r) = quotRem n $ permutationSize (length xs - 1)
      (as, b : bs) = splitAt q xs
 
problem_24 :: [Int]
problem_24 = permutationCount [0 .. 9] (1000000 - 1)
 