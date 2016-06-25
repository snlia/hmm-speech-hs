module Vad  
( vad
) where

import Data.WAVE
import Data.List
import Common
import Enframe

vad :: WAVE -> WAVE
vad wav = 
    WAVE {waveHeader = waveHeader wav, waveSamples = vad' $ y fs}
        where fs = waveFrameRate . waveHeader wav
              y = mkframe $ waveSamples wav framelen frameinc
              framelen = fs / 50;
              frameinc = framelen / 2;
vad' :: Frames -> FrameRate -> WAVESamples
vad' y fs = takefrom headp endp y
    where takefrom a b = drop a . take b
          headp = vadHead y fs
          endp = vadEnd y fs

--getting short-term Energy
sE :: WAVESamples -> FrameRate -> Frame
sE y fs = map double $ ones yy

--Find head point
vadHead :: WAVESamples -> FrameRate -> Count
vadHead y fs = len . takeWhile (< mH) se
    where mH = (maximum $ se y fs) / 4.0
          mL = (maximum $ se y fs) / 4.0
          se = sE y

--Find end point
vadEnd :: WAVESamples -> FrameRate ->Count
