module ComplexFunctions where

import Data.Complex
import RealFunctions

fibonacci :: (Fractional a, Floating a, Num a) => a -> a 
fibonacci z = ((phi ** z) - (cos (pi*z)) * (phi ** (-z))) / (sqrt 5)
    where phi = (1 + (sqrt 5)) / 2

weierstrass :: (Floating a) => Int -> a -> a -> a -> a
weierstrass limit a b x = sum $ map (\n -> (a^n) * cos (b^n * pi * x)) [0..limit]

foldPhase :: (RealFloat a) => a -> Complex a -> Complex a
foldPhase p z = mkPolar (magnitude z) (triangle (phase z) (p) (p)) 

foldMag :: (RealFloat a) => a -> Complex a -> Complex a
foldMag p z = mkPolar (triangle (magnitude z) p p) (phase z)

foldX :: (RealFloat a) => a -> Complex a -> Complex a
foldX p z = (x:+y)
    where x = triangle (realPart z) p p 
          y = imagPart z

foldY :: (RealFloat a) => a -> Complex a -> Complex a
foldY p z = (x:+y)
    where x = realPart z
          y = triangle (imagPart z) p p 


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

diamond :: (RealFloat a, Fractional a) => Complex a -> a
diamond z = y + x
    where x = abs (realPart z)
          y = abs (imagPart z)

compareBy :: (Ord a, RealFloat a) => (Complex a -> a) ->  Complex a -> Complex a -> Ordering
compareBy func z1 z2 = compare (func z1) (func z2)
{--
hofstadterQ :: (Integral a) => a -> a
hofstadterQ 1 = 1
hofstadterQ 2 = 1
hofstadterQ n = (hofstadterQ (n - hofstadterQ (n - 1))) + 
    (hofstadterQ (n - hofstadterQ (n - 2)))
--}
