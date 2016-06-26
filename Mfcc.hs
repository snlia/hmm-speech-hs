module Mfcc 
(
    mfcc
) where

import Common
import Data.Complex
import Mel
import Numeric.FFT

mfcc :: Frames -> FrameRate -> [Frame]
mfcc f fs = y_dct
    where 
        y_dct = fmap dct y_mag :: [[Double]]
        y_ln = fmap (map log) y_mel :: [[Double]]
        y_mel =  fmap (calcMel melbank) y_mag :: [[Double]]
        y_mag = fmap (map magnitude) y_fft :: [[Double]]
        y_fft = fmap fft $ transc y :: [[Complex Double]]
        melbank = mkmel ms flen fs
        y = samples f
        ms = 16
        flen = framelen f

dct :: Frame -> Frame
dct f = map funF [1..n]
    where n = length f
          funF u = sqrt (2 / (tof n)) * (sum $ map (funF' u) [1..n])
          funF' u x = (f !! x) * cos ((pi / (2 * (tof n))) * (2 * (tof x) + 1) * (tof u))
          tof x = fromIntegral x :: Double

