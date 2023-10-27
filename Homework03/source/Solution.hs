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
unique l = notElem (head l) (tail l) && unique (tail l)


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a * a + b * b == c * c]


primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c) | (a, b, c) <- pythagoreanTriples, gcd a b == 1 && gcd a c == 1 && gcd b c == 1]


isPerfect :: Integral a => a -> Bool
isPerfect a = sum [x | x <- [1..(a-1)], mod a x == 0] == a

perfectNumbers :: Integral a => [a]
perfectNumbers = [a | a <- [1..], isPerfect a]


generateCantorPairs :: Integral t => t -> t -> [(t, t)]
generateCantorPairs x y
  | x == 0 = (x, y):generateCantorPairs (y + 1) 0
  | otherwise = (x, y):generateCantorPairs (x - 1) (y + 1)

cantorPairs :: Integral a => [(a, a)]
cantorPairs = generateCantorPairs 0 0


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
