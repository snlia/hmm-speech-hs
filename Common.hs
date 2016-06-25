module Common
( FrameInc,
  FrameLen,
  Frame,
  Frames (..),
  wav2data,
  one,
  ones,
  zero,
  zeros,
  toOne,
  toOnes,
  hamming
) where

import Data.WAVE

type FrameInc = Int
type FrameLen = Int
type FrameRate = Int
type Count = Int
type Frame = [Double]
data Frames = Frames {samples :: [Frame], frameinc :: FrameInc, framelen :: FrameLen} deriving (Show)

--trans WAVESample  to Double
int2double :: WAVESample -> Double
int2double x = fromIntegral x :: Double

--trans WAVESamples to Frames
wav2data :: WAVESamples -> Frames
wav2data x = Frames {samples = fmap (map int2double) x, framelen = flen, frameinc = -1}
    where flen = length $ head x 

--output a list of 1.0
one :: Count -> Frame
one = flip replicate $ 1.0
ones :: Count -> Count-> Frames
ones a b = Frames {samples = replicate a . one $ b, framelen = b, frameinc = -1}

--output a list of 0.0
zero :: Count -> Frame
zero = flip replicate $ 0.0
zeros :: Count -> Count-> Frames
zeros a b = Frames {samples = replicate a . zero $ b, framelen = b, frameinc = -1}

--make max value to 1
toOne :: Frame -> Frame
toOne x = map (flip (/) . maxV $ x) x
    where maxV = maximum . (map abs)
toOnes :: Frames -> Frames
toOnes x = Frames {samples = map toOne samp, framelen = flen, frameinc = finc}
    where samp = samples x
          flen = framelen x
          finc = frameinc x

--output a hamming window
hamming :: Count -> Frame
hamming count = map (\x -> 0.54 - 0.46 * cos (2 * pi * x / cx))  [1..cx] 
    where int2float x = fromIntegral x :: Double
          cx = int2float count
