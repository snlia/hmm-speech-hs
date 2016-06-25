module Vad  
( vad
) where

import Data.WAVE
import Common
import Enframe

vad :: WAVE -> WAVE
vad wav = 
    WAVE {waveHeader = waveHeader wav, waveSamples = vad' $ waveSamples wav}
vad' :: WAVESamples -> WAVESamples
vad' y =
    drop vadHead . take $ vadEnd y y

--Find head point
vadHead :: WAVESamples -> Int
vadHead y = 

--Find end point
vadEnd :: WAVESamples -> Int
