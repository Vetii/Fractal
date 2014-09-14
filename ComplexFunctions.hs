module ComplexFunctions where

import Data.Complex
import RealFunctions

fibonacci :: (Fractional a, Floating a, Num a) => a -> a 
fibonacci z = ((phi ** z) - (cos (pi*z)) * (phi ** (-z))) / (sqrt 5)
    where phi = (1 + (sqrt 5)) / 2

fold :: (RealFloat a) => a -> Complex a -> Complex a
fold p z = mkPolar (magnitude z) (triangle (phase z) (p) (p)) 

-- remaps x and y in [-1,1] 
remap :: (RealFloat a) => Complex a -> a -> a -> Complex a 
remap z w h = ((realPart z') / a) :+ ((imagPart z') / a)
    where a = 0.5 * (min w h)
          z'= z - (w/2 :+ h/2) -- centering

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
