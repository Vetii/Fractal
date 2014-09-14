module Julia where

import Data.Complex
import System.Random
import Data.List

-- JULIA MATH-----
-- A generalized Julia Function
superJulia :: (RealFloat a, Fractional a) =>  [Complex a] -- parameters
  -> Complex a -- starting point.
  -> Complex a
superJulia [c] z = c
superJulia (c:cs) z = c * z^(length cs) + superJulia cs z

-- Some functions that look cool.

{--
snowflake :: (RealFloat a, Fractional a) => a -> Complex a -> Complex a -> Complex a
    -> Complex a -> Complex a
    --}
snowflake p a b c z = (abs (a * (z ** p))) * b + c

flake :: (RealFloat a, Fractional a) => Complex a -> Complex a
flake z = z**6 + (0.883 * (cis(pi/4)))

newton :: (Floating a, Fractional a) => a -> a -> a
newton n z = (1 + (n - 1) * z**n) / (n * (z**(n-1)))

glynn :: (RealFloat a, Fractional a) => Complex a -> Complex a
glynn z = z**1.5 - 0.2

julia7 :: (RealFloat a, Fractional a) => Complex a -> Complex a
julia7 z = z**3 + 0.400 

dragon :: (RealFloat a, Fractional a) =>  Complex a -> Complex a
dragon z = z**2 + ((-0.835) :+ 0.2321)

sshaped :: (RealFloat a, Fractional a) =>  Complex a -> Complex a
sshaped z = z**2 + (0.32:+0.043)

pierced :: (RealFloat a, Fractional a) =>  Complex a -> Complex a
pierced z = z**2 + ((-0.74543):+0.11301)

liquidSpirals :: (RealFloat a, Fractional a) =>  Complex a -> Complex a
liquidSpirals z = z**2 + ((-0.1):+0.651)

julia1 :: (RealFloat a, Fractional a) => Complex a -> Complex a
julia1 z = z**2 + ((-0.156844471694257101941):+(-0.649707745759247905171))

julia2 :: (RealFloat a, Fractional a) => Complex a -> Complex a
julia2 z = z**2 + ((-0.4):+0.6)

julia3 :: (RealFloat a, Fractional a) => Complex a -> Complex a
julia3 z = 1 - (z**2) + (z**5) / (2 + 4 * z) + (0.4:+0.6)

impossible :: (RealFloat a, Fractional a) => Complex a -> Complex a
impossible z = (sinh(z**2)**0.5) + (0.065:+0.122)

impossible2 :: (RealFloat a, Fractional a) => Complex a -> Complex a
impossible2 z = ((z**2+z)/log(z))+(0.268:+0.060)

superPattern :: (RealFloat a, Fractional a) =>  Complex a -> Complex a
superPattern z = (exp (z**3)) - 0.59 
