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
width = 600

height :: (Num a) => a
height = 600


pixelRenderer :: (RealFloat a) => Int -> Int -> a 
pixelRenderer x y = smooth samples -- avg' (Prelude.map (f) samples)
    where samples = sample 128 (magnitudeLimit 2) dragon start
          start   = afold (remapped x y)
          afold   = (1.5*(cis(pi/(4)))*).(foldPhase (pi/6))
          f z     = cos (atan2 (imagPart z) (realPart z)) --(cos . abs . phase) z 
          f1 x z  = 1 + (z*x)
          f2 x z  = 1 - (z*x)

paramRenderer :: (RealFloat a) => a -> Int -> Int -> a
paramRenderer progress x y = smooth samples -- progress between 0 and 100% 
    where samples = sample 128 (magnitudeLimit 2) (dragon) start
          start   = ((cis n *).(foldPhase (pi/6))) (remapped x y)
          n       = progress * 2 * pi

remapped :: (RealFloat a) => Int -> Int -> Complex a
remapped x y = (remap (fromIntegral x :+ fromIntegral y) width height)

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

-- render a frame with progress between 0% and 100%
renderFrame :: (Source r a, RealFloat a, Elt a) => Array r DIM2 a -> a -> Array D DIM2 a
renderFrame !input progress = traverse input id (\_ (Z :. y :. x) -> paramRenderer progress x y) 

convert :: (Shape sh) => Array U sh Double
     -> Array D sh (GHC.Word.Word8, GHC.Word.Word8, GHC.Word.Word8)
convert !array = Repa.map (rgb8OfGreyDouble) array

getMinimum :: (Ord b, Num b, Source r b, Elt b, Unbox b, Shape sh) => 
    Array r sh b -> IO (b) 
getMinimum !array = foldAllP (min) 100000 array

getMaximum :: (Ord b, Num b, Source r b, Elt b, Unbox b, Shape sh) =>
    Array r sh b -> IO (b)
getMaximum !array = foldAllP (max) 0 array

correctExpo :: (Source r b, Unbox b, Elt b, Floating b, Shape sh, RealFrac b) => 
    Array r sh b -> b -> b -> IO (Array U sh b) 
correctExpo !array !mini !maxi = computeP $ Repa.map ((map' mini maxi 0.0 1.0)) array
--
compute :: IO ()
compute = do
    img <- computeP (render canvas) :: IO (Array U DIM2 Double) -- super intensive
    mini <- getMinimum img
    maxi <- getMaximum img
    corrected <- correctExpo img mini maxi :: IO (Array U DIM2 Double)
    image <- computeP (convert corrected) 
    writeImageToBMP "out.bmp" image

computeFrame :: Double -> Int -> IO ()
computeFrame progress frameNum = do
    img <- computeP (renderFrame canvas progress) :: IO (Array U DIM2 Double) -- super intensive
    mini <- getMinimum img
    maxi <- getMaximum img
    corrected <- correctExpo img mini maxi :: IO (Array U DIM2 Double)
    image <- computeP (convert corrected) 
    let filename = "gifs/" Data.List.++ (show frameNum) Data.List.++ ".bmp"
    writeImageToBMP filename image

main :: IO () 
main = compute -- foldl (>>) (return ()) (Data.List.zipWith3 ($) (repeat computeFrame) progresses [1..])
    where progresses = takeWhile (<= 1.0) $ iterate (+0.005) 0.0
