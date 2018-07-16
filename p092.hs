import Data.Array
import Data.Char
import Data.List
makeIncreas 1 minnum  = [[a]|a<-[minnum..9]]
makeIncreas digits minnum  = [a:b|a<-[minnum ..9],b<-makeIncreas (digits-1) a]
squares :: Array Char Int
squares = array ('0','9') [ (intToDigit x,x^2) | x <- [0..9] ]
 
next :: Int -> Int
next = sum . map (squares !) . show
factorial n = if n == 0 then 1 else n * factorial (n - 1)  
countNum xs=ys
    where
    ys=product$map (factorial.length)$group xs
yield :: Int -> Int
yield = until (\x -> x == 89 || x == 1) next
problem_92=
    sum[div p7 $countNum a|
    a<-tail$makeIncreas  7 0,
    let k=sum $map (^2) a,
    yield k==89
    ]
    where
    p7=factorial 7

