module Common
( FrameInc,
  FrameLen,
  DATASample,
  DATASamples,
  one,
  ones,
  zero,
  zeros,
  toOne,
  toOnes,
  hamming
) where

type FrameInc = Int
type FrameLen = Int
type Count = Int
type DATASample = [Double]
type DATASamples = [[Double]]

--output a list of 1.0
one :: Count -> DATASample
one = flip replicate $ 1.0
ones :: Count -> Count-> DATASamples
ones a b = replicate a $ one b

--output a list of 0.0
zero :: Count -> DATASample
zero = flip replicate $ 0.0
zeros :: Count -> Count-> DATASamples
zeros a b = replicate a $ zero b

--make max value to 1
toOne :: DATASample -> DATASample
toOne x = map (flip (/) . maxV $ x) x
    where maxV = maximum . (map abs)
toOnes :: DATASamples -> DATASamples
toOnes x = map toOne x


--output a hamming window
hamming :: Count -> DATASample
hamming count = map (\x -> 0.54 - 0.46 * cos (2 * pi * x / cx))  [1..cx] 
    where int2float x = fromIntegral x :: Double
          cx = int2float count
