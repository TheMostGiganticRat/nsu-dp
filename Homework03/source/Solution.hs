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
unique (x:tail) = if x `elem` tail then False else unique tail

pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a , b, c) | a <- [1,2..], b <- [1,2..], c <-[1,2..], a^2 + b^2 == c^2 ]

primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(m*m - n*n, 2*m*n, m*m + n*n) | m <- [2,3..], n <- [1..m-1], gcd m n == 1, odd (m - n)] 


perfectNumbers :: Integral a => [a]
perfectNumbers = [a | a <- [2..], sum (f a) == a]

f :: Integral a => a -> [a]
f x = [a | a <- [1..x-1], x `mod` a == 0]

cantorPairs :: Integral a => [(a, a)]
cantorPairs = undefined


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
