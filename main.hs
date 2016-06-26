import System.IO
import Data.WAVE
import Common 
import Enframe 
import Vad

type Wfilename = String
type Wname = String
type Wno = Int
type Wword = String

getwav :: Wname -> Wword -> Wno -> IO WAVE
getwav a b c = do 
    hwav <- openFile (getwavname a b c) ReadMode
    hGetWAVE hwav
    where getwavname a b c = pwd ++ a ++ "_" ++ b ++ "_" ++ (show c) ++ ".wav"
          pwd = "/home/snlia/work/speech/myspeech-hs/data/"

main = do
    let wname = "14307130244"
        wword = "Start"
        wno = 1
    x <- getwav wname wword wno
    handle <- openFile "1.wav" WriteMode
    let y = toOnes $ mkframe (waveSamples x) framelen frameinc
        ys = samples y
        fs = 8000
        framelen = fs `div` 50
        frameinc = framelen `div` 2
    hPutWAVE handle (vad x)
