{- 
 - Module containing a few functions about rendering the fractals.
-}
module Rendering where

import Data.Complex
import Data.List

-- RENDERING ----
escape :: (RealFloat a, Fractional a) => [Complex a] -- list of samples
    -> (a -> a) -- Intensity mapping function 
    -> a -- pixel brightness
escape samples mapFunc = sum $ zipWith (\x y -> mapFunc (magnitude (y - x)))
     samples (tail samples)

follow :: (RealFloat a, Fractional a) => [Complex a] -- list of samples
    -> (Complex a -> a) -- itensity function
    -> a
follow [] func = 0
follow samples func = func $ last samples

smooth :: (RealFloat a, Fractional a) => [Complex a]
    -> a 
smooth samples = foldl' (+) 0 (map (exp . ((-1)*) . magnitude) samples)

-- to get all values from iterating Julia
sample :: (RealFloat a, Fractional a) => Int  -- nbSamples
    -> (Complex a -> Bool) -- stopping condition
    -> (Complex a -> Complex a) -- how to get next point in serie
    -> Complex a -- starting point of serie (z0)
    -> [Complex a] -- list of samples
sample nbsamples isValid funct start = (takeWhile isValid).(take nbsamples).
    iterate funct $ start


