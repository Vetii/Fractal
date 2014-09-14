module Mobius where

import Data.Complex

moebius :: (Floating a, Fractional a, Eq a) => a -> a -> a -> a -> a -> a
moebius a b c d z 
    | a*d - b*c == 0 = undefined
    | otherwise = (a*z + b) / (c*z + d)

lambdamoebius :: (RealFloat a, Fractional a, Eq a) => 
    Complex a -> Complex a -> Complex a
lambdamoebius a z = (z - a) / (1 - ((conjugate a) * z))

circular :: (RealFloat a, Fractional a) => Complex a -> Complex a
circular z = möbius 0 (-1) 1 0 z

elliptic :: (Floating a, Fractional a, Eq a) => a -> a -> a
elliptic theta z = möbius (cos a) (-sin a) (sin a) (cos a) z --(cis theta) * z
    where a = theta

parabolic :: (Floating a, Fractional a) => a -> a -> a
parabolic a z = z + a -- translation

hyperbolic :: (Floating a, Fractional a) => a -> a -> a
hyperbolic theta z = (exp theta) * z

loxodromic :: (Floating a, Fractional a) => a -> a -> a 
loxodromic a z = a * z -- S-shaped path

projection :: (Floating a, Fractional a) => a -> a  
projection z = tanh 
