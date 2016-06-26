module Mel
(
    mkmel,
    calcMel
) where

import Common

data Mel = Mel {fm :: (Int, Int), mel :: [Double]} deriving (Show)
type Mels = [Mel]

--Change x to Double
toF :: (Integral x) => x -> Double
toF x = fromIntegral x :: Double

--calc a Frame's mel
calcMel :: Mels -> Frame -> Frame
calcMel [] _ = []
calcMel (x:ms) f = sum (zipWith (*) f' xx) : calcMel ms f
    where f' = takefrom (fm x) f
          xx = mel x
          takefrom (a, b) = drop a . take b

bf :: Double -> Double
bf f = 2595 * (logBase 10 (1 + f / 700))

bf' :: Double -> Double
bf' c = (10 ** (c / 2595) - 1) * 700

fc :: FrameLen -> Count -> FrameRate -> Int -> Double
fc n mM fs m = (toF n / toF fs) * (bf' (bf fl + toF m * (bf (fh) - bf (fl)) / toF (mM + 1)))
    where fh = 0.5
          fl = 0

hm :: FrameLen -> Count -> FrameRate -> Int -> Mel
hm n mM fs m = Mel {fm = (floor stp, floor edp), mel = mf}
    where f = floor . (fc n mM fs)
          stp = toF $ f (m - 1)
          mdp = toF $ f m
          edp = toF $ f (m + 1)
          mf = upp ++ downp
          upp = map (\k -> (k - stp) / (mdp - stp)) [stp .. mdp]
          downp = map (\k -> (edp - k) / (edp - mdp)) [(mdp + 1) .. edp]

--make a bank of mels
mkmel :: Count -> FrameLen -> FrameRate -> Mels 
mkmel m n fs = map (hm n m fs) [1..m]
