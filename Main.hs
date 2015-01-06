module Main where

import Move
import Sound
import Move
import Sound
import Robotics.NXT
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent


main :: IO ()
main = withNXT "/dev/rfcomm0" logic 
        where
          logic = do setInputModeConfirm One SoundDB RawMode
                     liftIO $ threadDelay 100000
                     liftIO $ print "Started Sampling"
                     initInVals <- replicateM cuttime (liftIO  (threadDelay samplerate) >> getInputValues One)
                     let initRawVals = map getRawADValue initInVals
                     values <- gatherData initRawVals
                     let duration = valuesToDuration values
                         morse = dropRubbish $ reverse $ durationToMorse duration
                         morseForAlphabetConv = cleanFromBspaces morse
                     liftIO $ print morseForAlphabetConv
                     liftIO $ print (decodeSentence morseForAlphabetConv)
                     mapM_ morseToMoveAction morse
