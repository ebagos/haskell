{-
5000個以上の名前が書かれている46Kのテキストファイル names.txt を用いる. まずアルファベット順にソートせよ.

のち, 各名前についてアルファベットに値を割り振り, リスト中の出現順の数と掛け合わせることで, 名前のスコアを計算する.

たとえば, リストがアルファベット順にソートされているとすると, COLINはリストの938番目にある. またCOLINは 3 + 15 + 12 + 9 + 14 = 53 という値を持つ. よってCOLINは 938 × 53 = 49714 というスコアを持つ.

ファイル中の全名前のスコアの合計を求めよ.
-}

module Lib
    ( someFunc
    ) where

import Data.List
import Data.Char
        
someFunc :: IO ()
someFunc = do
    input <- readFile "names.txt"
    let names = sort $ read $ "[" ++ input ++ "]"
    let scores = zipWith score names [1..]
    print . sum $ scores
        where score w i = (i *) . sum . map (\c -> ord c - ord 'A' + 1) $ w
    
