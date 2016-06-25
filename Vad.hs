module Vad  
( vad,
  vadHead
) where

import Data.WAVE
import Data.List
import Common
import Enframe

vad :: WAVE -> WAVE
vad wav = 
    WAVE {waveHeader = waveHeader wav, waveSamples = takefrom (vad' y) samp}
        where fs = waveFrameRate . waveHeader $ wav
              y = mkframe (waveSamples wav) framelen frameinc
              framelen = fs `div` 50
              frameinc = framelen `div` 2
              samp = waveSamples wav
              takefrom (a, b) = drop a . take b
vad' :: Frames -> (Int, Int)
vad' y =  (headp, endp)
    where headp = vadHead y
          endp = vadEnd y

--getting short-term Energy
sE :: Frames -> Vect
sE y = map (sum . (map sqr)) yy
    where sqr x = x * x
          yy = samples y

--getting short-time zero crossing rate
sZcr :: Frames -> Vect
sZcr y = map (sum . delta) yy
    where yy = samples y
          delta x = map abs $ zipWith (-) x $ tail x

--Find head point
vadHead :: Frames -> Count
vadHead y = inc * (length thdk)
    where mH = (maximum se) / 4.0
          mL = (maximum se) / 16.0
          z0 = (maximum szcr) * 0.08;
          szcr = sZcr y
          se = sE y
          fstk = takeWhile (< mH) se
          sndk = dropWhile (> mL) $ reverse fstk
          thdk = dropWhile (> z0) $ reverse (take (length sndk) szcr)
          inc = frameinc y

--Find end point
vadEnd :: Frames -> Count
vadEnd = vadHead . (frmap reverse)
