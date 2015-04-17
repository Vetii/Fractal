module RealFunctions where

import Data.Fixed -- mod'

-- one dimensional functions

sawtooth :: (RealFloat a) => a -> a
sawtooth = tan . abs . sin

triangle :: (RealFloat a) => a -> a -> a -> a
triangle x a p = (a / p) * (p - abs(x `mod'` (2*p) - p))

gamma :: (Floating a) => a -> a
gamma z = z ** (1/2.2)

smoothstep :: (RealFrac a) => a -> a -> a -> a
smoothstep min max x = 3 * (x'^2) - 2 * (x'^3) 
    where x' = map' min max 0.0 1.0 x 

decreasing :: (RealFloat a, Fractional a) => a -> a
decreasing x = 1/(x+1)

towards1 :: (Floating a) => a -> a
towards1 x = exp (-1 / (abs x)) 

linear :: Num a => a -> a
linear _ = 1

schwefel :: (Floating a) => a -> a
schwefel z = 2 * 418.9829 - (f z)
    where f x = x * sin(sqrt(abs x))

reflection :: (Floating a, Fractional a) => a -> a
reflection z = 1 / z

map' :: RealFrac a => a -> a -> a -> a -> a -> a
map' inMin inMax outMin outMax x = (x - inMin) * (outMax - outMin) / (inMax - inMin) + outMin

clamp :: (Ord a) => a -> a -> a -> a
clamp min max x 
    | x < min = min
    | x > max = max
    | otherwise = x
