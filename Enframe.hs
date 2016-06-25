module Enframe  
( enframe,
  enframe_mult,
) where

import Common

enframe :: DATASample -> DATASample -> DATASample
enframe a b = zipWith (*) a b

enframe_mult :: DATASample -> DATASample -> FrameLen -> FrameInc -> DATASamples
enframe_mult [] _ _ _ = [] 
enframe_mult a b len inc = enframe (take len a) b : enframe_mult (drop inc a) b len inc
    
