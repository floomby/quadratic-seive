module Trial (trialFactorize) where

import Data.List

import Atkin

divInto :: Integral a => a -> a -> a
divInto n x
    | (gcd x n) == 1    = 0
    | otherwise         = 1 + (divInto (n `quot` x) x)

trialFactorizei :: Integral a => a -> [a]
trialFactorizei n =
    map (divInto n) $ primes n
    
trialFactorize :: Integral a => a -> [a]
trialFactorize n =
    (reverse . dropWhile (== 0) . reverse) $ trialFactorizei n