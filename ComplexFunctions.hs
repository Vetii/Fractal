module ComplexFunctions where

import Data.Complex
import RealFunctions

fibonacci :: (Fractional a, Floating a, Num a) => a -> a 
fibonacci z = ((phi ** z) - (cos (pi*z)) * (phi ** (-z))) / (sqrt 5)
    where phi = (1 + (sqrt 5)) / 2

fold :: (RealFloat a) => Complex a -> Complex a
fold z = mkPolar (magnitude z) (triangle (phase z) (p) (p)) 
    where p = pi/6

-- STOP CONDITIONS
isNotInfinite :: (RealFloat a, Fractional a ) => Complex a -> Bool
isNotInfinite z = not $ (isNaN . magnitude) z || (isInfinite . magnitude) z

magnitudeLimit :: (Ord a, RealFloat a, Fractional a) => a -> Complex a -> Bool
magnitudeLimit limit z = magnitude z < limit

{--
hofstadterQ :: (Integral a) => a -> a
hofstadterQ 1 = 1
hofstadterQ 2 = 1
hofstadterQ n = (hofstadterQ (n - hofstadterQ (n - 1))) + 
    (hofstadterQ (n - hofstadterQ (n - 2)))
--}
