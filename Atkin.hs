module Atkin (primes) where

import Data.List

checkPolys :: Integral a => (a, a) -> a -> [(a, Bool)]
checkPolys (x, y) limit = [(poly1 (x, y), (check1 (x, y) (poly1 (x, y)))),(poly2 (x, y), (check2 (x, y) (poly2 (x, y)))),(poly3 (x, y), (check3 (x, y) (poly3 (x, y))))]
    where poly1 (x, y) = 4 * x ^ 2 + y ^ 2
          poly2 (x, y) = 3 * x ^ 2 + y ^ 2
          poly3 (x, y) = 3 * x ^ 2 - y ^ 2
          check1 (x, y) n = (n <= limit) && ((n `mod` 12) `elem` [1,5])
          check2 (x, y) n = (n <= limit) && (n `mod` 12 == 7)
          check3 (x, y) n = (n <= limit) && (x > y) && (n `mod` 12 == 11)

primesi :: Integral a => a -> [a]
primesi n = concat $ filter (odd . length) $ group $ sort $ map fst $ filter (\(_, x) -> x) $ concat $ map (\(x, y) -> checkPolys (x, y) n) (tmp n)

minus :: Integral a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus l1@(x:xs) l2@(y:ys)
    | x > y     = minus l1 ys
    | x < y     = x : minus xs l2
    | otherwise = minus xs l2

    
primes :: Integral a => a -> [a]
primes n = [2, 3] ++ (xs `minus` exps)
    where top = (floor . sqrt . fromIntegral) n
          xs = primesi n
          lit = filter (<= top) xs
          exps = sort $ concat $ map (expToLim n) lit

expToLim :: Integral a => a -> a -> [a]
expToLim lim num = takeWhile (<= lim)[ x * num ^ 2 | x <- [1..]]
  
tmp :: Integral a => a -> [(a, a)]
tmp n = [ (x, y) | x <- [1..((floor . sqrt . fromIntegral) n)], y <- [1..((floor . sqrt . fromIntegral) n)]]