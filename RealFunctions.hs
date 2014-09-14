module RealFunctions where

import Data.Fixed -- mod'

-- one dimensional functions

sawtooth :: (RealFloat a) => a -> a 
sawtooth = tan . abs . sin 

triangle :: (RealFloat a) => a -> a -> a -> a
triangle x a p = (a / p) * (p - abs(x `mod'` (2*p) - p))

gamma :: (Floating a) => a -> a
gamma z = z ** (1/2.2)

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
