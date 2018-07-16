{-
正の整数に以下の式で繰り返し生成する数列を定義する.

    n → n/2 (n が偶数)

    n → 3n + 1 (n が奇数)

13からはじめるとこの数列は以下のようになる.
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

13から1まで10個の項になる. 
この数列はどのような数字からはじめても最終的には 1 になると考えられているが, まだそのことは証明されていない(コラッツ問題)

さて, 100万未満の数字の中でどの数字からはじめれば最長の数列を生成するか.

注意: 数列の途中で100万以上になってもよい
-}

module Lib
    ( someFunc
    ) where
{-
import Data.Ord (comparing)
import Data.List (maximumBy)
-}
someFunc :: IO ()
someFunc = do
    putStr "(max lehgth, value) = "
    print $ {-maximum-} problem_14

{-その１ -}
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
        | even n = [n] ++ (collatz $ n `div` 2)
        | otherwise = [n] ++ (collatz $ (n * 3 + 1))

collatzLength :: [Int] -> [Int]
collatzLength xs = map length $ map collatz xs

getResult :: (Int, Int) -> Int -> [Int] -> (Int, Int)
getResult rc _ [] = rc
getResult rc n (x:xs)
        | (fst rc) < x = getResult (x, n) (n + 1) xs
        | otherwise = getResult rc (n + 1) xs

problem_14 = getResult (0,0) 1 $ collatzLength [1..999999]

{- その２ 本問題を解くという点では一番良い
collatz :: Int -> Int -> Int
collatz 1 c = c
collatz n c
        | odd n = collatz (n * 3 + 1) (c + 1)
        | otherwise = collatz (div n 2) (c + 1)

collatzMax :: [Int] -> [Int]
collatzMax [] = []
collatzMax (x:xs) = collatz x 1 : collatzMax xs

problem_14 = maximum $ collatzMax [2..999999]
-}
{- その３：処理時間かかりすぎ
maxCollatz :: Integral a => a -> a
maxCollatz n = last $ maximumBy (comparing length) $ map collatz [n, (n - 1)..1]

-- nからはじまるコラッツ数列(逆順)
collatz :: Integral a => a -> [a]
collatz n = collatz' n [n]
  where
    collatz' 1 ns = ns
    collatz' n ns = collatz' next $ next:ns
      where next | even n    = n `div` 2
                 | otherwise = 3 * n + 1

problem_14 = maxCollatz 999999                
-}
