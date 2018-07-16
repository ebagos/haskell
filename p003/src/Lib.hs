{-
13195 の素因数は 5, 7, 13, 29 である
600851475143 の素因数のうち最大のものを求めよ
-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = print $ problem_3

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
 where
   factor n (p:ps) 
       | p*p > n        = [n]
       | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
       | otherwise      =     factor n ps

problem_3 = last (primeFactors 600851475143)

