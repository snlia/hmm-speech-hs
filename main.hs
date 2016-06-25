import System.IO
import Data.WAVE
import Vad

type Wfilename = String
type Wname = String
type Wno = Int
type Wword = String

main = do
    let getwavname a b c = pwd ++ a ++ "_" ++ b ++ "_" ++ (show c) ++ ".wav"
        pwd = "/home/snlia/work/speech/myspeech-hs/data/"
        wname = "14307130244"
        wword = "Start"
        wno = 1
    hwav <- openFile (getwavname wname wword wno) ReadMode
    x <- hGetWAVE hwav
    print $ waveSamples $ vad x
