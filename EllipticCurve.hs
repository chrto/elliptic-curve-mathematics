module EllipticCurve where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- Elliptic curve parameters
modulus :: Integer
modulus = 47  -- p = 47

a :: Integer
a = 0         -- a = 0 for the curve y^2 = x^3 + ax + b

b :: Integer
b = 7         -- b = 7 for the curve y^2 = x^3 + 7 mod 47

-- Point on the elliptic curve (Infinity is represented by Nothing)
data Point = Point Integer Integer | Infinity deriving (Eq, Ord)

instance Show Point where
  show :: Point -> String
  show Infinity = "(0,0)"
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- Elliptic Curve Mathematics
-- 1) Modular inverse
-- 2) Point addition
-- 3) Point doubling
-- 4) Scalar multiplication

-- 1) Modular inverse
-- Modular inverse using the Extended Euclidean Algorithm
modularInverse :: Integer -> Integer -> Maybe Integer
modularInverse a m
  | a == 0    = Nothing
  | a < 0     = let a' = a `mod` m                    -- a' is positive equivalent of a under modulo m
                in modularInverse (a' `mod` m) m      -- compute the modular inverse of a' as a positive number
  | otherwise = if g == 1
                then Just (i `mod` m)
                else Nothing
                where
                  (i, _, g) = eea a m
                  eea 0 m = (0, 1, m)
                  eea a m = let (q, r) = m `quotRem` a
                                (s, t, g) = eea r a
                            in (t - q * s, s, g)

-- modular inverse of 3 should be 16
--- >>> Just 16 == modularInverse 3 47
-- True

-- modular inverse of 13 should be 29
--- >>> Just 29 == modularInverse 13 47
-- True

-- modular inverse of 100 should be 8
--- >>> Just 8 == modularInverse 100 47
-- True

--- >>> Just 37 == modularInverse (-33) 47
-- True

--- >>> Nothing == modularInverse 0 47
-- True

-- 2) Point addition on the elliptic curve
pointAdd :: Point -> Point -> Point
pointAdd Infinity p = p
pointAdd p Infinity = p
pointAdd (Point x1 y1) (Point x2 y2)
  | x1 == x2 && y1 == (-y2) `mod` modulus = Infinity                    -- P + (-P) = O (Identity element)
  | x1 == x2 && y1 == y2                  = pointDouble (Point x1 y1)   -- P + P (Doubling)
  | otherwise = Maybe.fromMaybe Infinity $ pure Point <*> mx3 <*> my3
                where
                  mλ :: Maybe Integer = (\mi -> ((y1 - y2) * mi) `mod` modulus) <$> modularInverse (x1 - x2) modulus
                  mx3 :: Maybe Integer = (\λ -> (λ * λ - x1 - x2) `mod` modulus) <$> mλ
                  my3 :: Maybe Integer = (\λ x3 -> (λ * (x1 - x3) - y1) `mod` modulus) <$> mλ <*> mx3

-- [17, 19] + [46, 37] should be [32, 43]
--- >>> (Point 32 43) == pointAdd (Point 17 19) (Point 46 37)
-- True

-- [20, 39] + [32, 4] should be [43, 32]
--- >>> (Point 43 32) == pointAdd (Point 20 39) (Point 32 4)
-- True

-- [17, 19] + [17, -19] should be Infinity
--- >>> Infinity == pointAdd (Point 17 19) (Point 17 (-19))
-- True


-- 3) Point doubling on the elliptic curve
pointDouble :: Point -> Point
pointDouble Infinity    = Infinity
pointDouble (Point x y) = Maybe.fromMaybe Infinity $ Point <$> mx3 <*> my3
                          where
                            mλ :: Maybe Integer = (\mi -> ((3 * x * x + a) * mi) `mod` modulus) <$> modularInverse (2 * y) modulus
                            mx3 :: Maybe Integer = (\λ -> (λ * λ - 2 * x) `mod` modulus) <$> mλ
                            my3 :: Maybe Integer = (\λ x3 -> (λ * (x - x3) - y) `mod` modulus ) <$> mλ <*> mx3

-- 2 * [17, 19] should be [8, 7]
--- >>> (Point 8 7) == pointDouble (Point 17 19)
-- True

-- 2 * [8, 7] should be [33, 6]
--- >>> (Point 33 6) == pointDouble (Point 8 7)
-- True

-- 2P should be P+P
--- >>> pointDouble (Point 17 19) == pointAdd (Point 17 19) (Point 17 19)
-- True

-- 4) Scalar multiplication using double-and-add
scalarMult :: Integer -> Point -> Point
scalarMult 0 _ = Infinity
scalarMult k p
  | k < 0     = scalarMult (-k) (negPoint p)  -- Handle negative scalar
  | otherwise = aux k p Infinity
                where
                  aux :: Integer -> Point -> Point -> Point
                  aux 0 _ acc = acc
                  aux n q acc
                    | n `mod` 2 == 1 = aux (n `div` 2) (pointDouble q) (pointAdd acc q)
                    | otherwise = aux (n `div` 2) (pointDouble q) acc

-- 2 * [13, 18] should be [16, 22]
--- >>> Point 16 22 == scalarMult 2 (Point 13 18)
-- True

-- 3 * [13, 18] should be [25, 13]
--- >>> (Point 25 13) == scalarMult 3 (Point 13 18)
-- True

-- 100 * [13, 18] should be [33, 6]
--- >>> (Point 33 6) == scalarMult 100 (Point 13 18)
-- True

negPoint :: Point -> Point
negPoint Infinity = Infinity
negPoint (Point x y) = Point x ((-y) `mod` modulus)

-- negation of [17, 19] should be [17, 28]
--- >>> (Point 17 28) == negPoint (Point 17 19)
-- True

-- negation of [17, 28] should be [17, 19]
--- >>> (Point 17 19) == negPoint (Point 17 28)
-- True

-- negation of [3, 38] should be [3, 9]
--- >>> (Point 3 9) == negPoint (Point 3 38)
-- True

-- Function to find points on the curve
findPointsInZp :: Set.Set Point
findPointsInZp = Set.fromList [
                  Point x y
                  | x <- [0 .. modulus - 1]
                  , y <- [0 .. modulus - 1]
                  , (y ^ 2 `mod` modulus) == curveRHS x
                ]
                    where
                      curveRHS :: Integer -> Integer
                      curveRHS x = (x ^ 3 + a*x + b) `mod` modulus

findPointsInZn :: Point -> Set.Set Point
findPointsInZn g = aux 0 g Set.empty
                   where
                    setSize :: Set.Set Point -> Integer
                    setSize = toInteger . Set.size
                    aux :: Integer -> Point -> Set.Set Point -> Set.Set Point
                    aux n g acc
                      | n  > setSize acc  = acc
                      | otherwise         = let g' = scalarMult n g
                                            in  aux (n + 1) g (Set.insert g' acc)

-- final proof
--- >>> findPointsInZp
-- fromList [(0,17),(0,30),(1,14),(1,33),(3,9),(3,38),(4,20),(4,27),(7,16),(7,31),(8,7),(8,40),(13,18),(13,29),(14,5),(14,42),(16,22),(16,25),(17,19),(17,28),(19,2),(19,45),(20,8),(20,39),(21,3),(21,44),(23,1),(23,46),(25,13),(25,34),(29,12),(29,35),(31,0),(32,4),(32,43),(33,6),(33,41),(35,21),(35,26),(39,23),(39,24),(43,15),(43,32),(44,11),(44,36),(46,10),(46,37)]

--- >>> Infinity == scalarMult 48 (Point 17 19)
-- True

--- >>> (Point 17 19) == scalarMult (48 + 1) (Point 17 19)
-- True

--- >>> scalarMult 100 (Point 17 19)
-- (33,6)

--- >>> modularInverse 100 47
-- Just 8

--- >>> scalarMult 8 (Point 33 6)
-- (0,17)

