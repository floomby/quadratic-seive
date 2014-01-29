module Sieve (sieve) where

import Data.List

import Atkin
import Trial

poly :: Integral a => a -> a -> a
poly n x = x^2 - n

--seive function specific to the QS
sieve :: Integral a => a -> a -> a -> [[a]]
sieve start base n =
    map trialFactorize $ take (length (primes base)) $ filter (smooth base) $ map (poly n) [start..]
    
smooth :: Integral a => a -> a -> Bool
smooth base n =
    (length (primes base)) >= (length (trialFactorize n))