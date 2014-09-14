{-# LANGUAGE BangPatterns #-}
-- import System.Environment
import Data.Array.Repa as Repa
import Data.Array.Repa.Eval as Eval
import Data.Array.Repa.IO.BMP as BMP
import Data.Array.Repa.Algorithms.Pixel as Pixel
import Data.Vector.Unboxed.Base as Unboxed

import Control.Monad.Identity

import GHC.Word
import Data.Complex
import Data.List
import Julia
import Rendering
import RealFunctions
import ComplexFunctions

width :: (Num a) => a
width = 5100

height :: (Num a) => a
height = 3300

pixelRenderer :: (RealFloat a) => Int -> Int -> a 
pixelRenderer x y = smooth samples -- avg' (Prelude.map (f) samples)
    --renderPixel samples decreasing
    where samples = sample 128 (magnitudeLimit 2) julia2 start
          start   = (fold (pi/6))(remap (fromIntegral x :+ fromIntegral y) width height)
          f z     = cos (atan2 (imagPart z) (realPart z)) --(cos . abs . phase) z 


avg :: (Num a, Fractional a) => [a] -> a
avg l = (sum l) / ((fromIntegral . length) l)

avg' :: (Num a, Fractional a) => [a] -> a
avg' = uncurry (/) . foldl' (\(s,n) x -> (s+x, n+1)) (0, 0)

-- 2D shaped array
shape :: DIM2 
shape = Z :. (height :: Int) :. (width :: Int) 

canvas :: Array U DIM2 Double
canvas = fromListUnboxed shape (take (fromIntegral (size shape)) (repeat 0))

render :: (Source r a, RealFloat a, Elt a) => Array r DIM2 a -> Array D DIM2 a
render !input = traverse input id (\_ (Z :. y :. x) -> pixelRenderer x y) 

convert :: (Shape sh) => Array D sh Double
     -> Array D sh (GHC.Word.Word8, GHC.Word.Word8, GHC.Word.Word8)
convert !array = Repa.map (rgb8OfGreyDouble) array

getMinimum :: (Ord b, Num b, Source r b, Elt b, Unbox b, Shape sh) => 
    Array r sh b -> b
getMinimum !array = runIdentity $ (foldAllP (min) 100000 array)

getMaximum :: (Ord b, Num b, Source r b, Elt b, Unbox b, Shape sh) =>
    Array r sh b -> b
getMaximum !array = runIdentity $ (foldAllP (max) 0 array)

-- super slow =(
correctExpo :: (Source r b, Unbox b, Elt b, Floating b, Shape sh, RealFrac b) => 
    Array r sh b -> Array D sh b
correctExpo !array = 
    let mini = getMinimum array
        maxi = getMaximum array
    in Repa.map (map' mini maxi 0.0 1.0) array
--
main :: IO () 
main = do 
    img <- computeP (render canvas) :: IO (Array U DIM2 Double) -- super intensive
    corrected <- return (correctExpo img) :: IO (Array D DIM2 Double)
    image <- computeP (convert corrected) 
    writeImageToBMP "out.bmp" image
