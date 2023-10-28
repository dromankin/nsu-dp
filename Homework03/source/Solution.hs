module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


unique :: Eq a => [a] -> Bool
unique [] = True
unique l = func l 
    where
        func [] = True
        func (x:xs) 
         | x `elem` xs    = False
         | otherwise = func xs


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a,b,c) |  c<-[1..],b<-[1..c],a<-[1..b], a^2+b^2==c^2]

nod 0 b = b
nod a b = nod (b `mod` a) a
primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a,b,c) |  c<-[1..],b<-[1..c],a<-[1..b], a^2+b^2==c^2, nod a b == 1]

divs a =sum[x | x<-[1..a-1], a `mod`x==0]
isPrime x |divs x ==1 = True
          |otherwise = False
primelist =[n | n<-[1..], isPrime n]
perfectNumbers :: Integral a => [a]
perfectNumbers =[2 ^ (x - 1) * (2 ^ x - 1) | x <- primelist, isPrime(2 ^ x - 1)]


cantorPairs :: Integral a => [(a, a)]
cantorPairs = [(x,i-x) | i<-[0..], x<-[i,i-1..0]]


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
