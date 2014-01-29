module Linear (uniform, nullspace) where

import Data.List

uniform :: Integral a => [[a]] -> [[a]]
uniform m =
    map (\x -> x ++ (take (target - (length x)) (repeat 0))) m
    where target = maximum $ map length m
    
nullspace :: Integral a => [[a]] -> [a]
nullspace m =
    