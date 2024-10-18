module Sandbox where

import EllipticCurve

import Control.Monad (when)

printAllPoints :: IO ()
printAllPoints = putStrLn ("Points on the curve y^2 = x^3 + ax + b mod p, where a=" ++ show a ++ ", b=" ++ show b ++ ", p=" ++ show modulus ++ ":")
  >> return findPointsInZp
  >>= \ps -> print "Number of points on the curve [Zp]: " >> print (length ps) >> print "Points on the curve" >> print ps

printPointsForGenerator :: Point -> IO ()
printPointsForGenerator g = putStrLn ("Points defined by generator G=" ++ show g ++ " on the curve y^2 = x^3 + ax + b mod p, where a=" ++ show a ++ ", b=" ++ show b ++ ", p=" ++ show modulus ++ ":")
  >> return (findPointsInZn g)
  >>= \ps -> print "Number of points on the curve [Zn]: " >> print (length ps) >> print "Points on the curve" >> print ps

test_find_match :: Maybe Integer -> Point -> IO ()
test_find_match Nothing q = test_find_match (pure 2) q
test_find_match (Just n) q = do
  let q' = scalarMult n q
      b = q' == q
  putStrLn ("Test with n=" ++ show n ++ ", Q'=" ++ show q' ++ ", Q=" ++ show q ++ ": " ++ show b)
  if b
    then putStrLn ("Test passed with n=" ++ show n)
    else test_find_match (pure (n + 1)) q

test_iterate_over :: Maybe Integer -> Point -> IO ()
test_iterate_over Nothing q = test_iterate_over (pure 2) q
test_iterate_over (Just n) q = do
  let q' = scalarMult n q
      b = q' == q
  Control.Monad.when b $ putStrLn ("Test with n=" ++ show n ++ ", Q'=" ++ show q'++ ", Q=" ++ show q ++ ": " ++ show b)
  test_iterate_over (pure (n + 1)) q

test_find_n :: Maybe Integer -> Point -> IO ()
test_find_n Nothing q = test_find_n (pure 2) q
test_find_n (Just n) q = do
  let q' = scalarMult n q
      b = q' /= Infinity
  if b
    then test_find_n (pure (n + 1)) q
    else putStrLn ("Test with n=" ++ show n ++ ", Q'=" ++ show q' ++ ", Q=" ++ show q ++ ": " ++ show b)
