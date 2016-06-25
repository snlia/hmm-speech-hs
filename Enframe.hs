module Enframe  
( enframe,
  enframe_mult,
  mkframe
) where

import Common
import Data.WAVE

--enframe a frame
enframe :: Frame -> Frame -> Frame
enframe a b = zipWith (*) a b

enframe_mult :: Frame -> Frame -> FrameLen -> FrameInc -> Frames
enframe_mult [] _ flen finc = Frames {samples = [], framelen = flen, frameinc = finc}
enframe_mult a b len inc = Frames {samples = enframe (take len a) b : nxsamp, framelen = len, frameinc = inc}
    where nxsamp = samples $ enframe_mult (drop inc a) b len inc
    
mkframe :: WAVESamples -> FrameLen -> FrameInc -> Frames
mkframe y len inc = enframe_mult (concat . samples . wav2data $ y) (one len) len inc
