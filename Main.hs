module Main where

import Robotics.NXT
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Data.List

data Morse = Dot | Dash | Space deriving Show


main :: IO ()
main = withNXT "/dev/rfcomm0" logic 
        where
          logic = do setInputModeConfirm One SoundDB RawMode
                     liftIO $ threadDelay 1000000
                     liftIO $ print "Started Sampling"
                     initInVals <- replicateM 200 ((liftIO $ threadDelay 100) >> getInputValues One)
                     let initRawVals = map getRawADValue initInVals
                     values <- gatherData initRawVals
                     let duration = valuesToDuration values
                         morse = durationToMorse duration
                     --liftIO $ print values
                     liftIO $ print $ length initInVals
                     liftIO $ print initRawVals
                     liftIO $ print $ length duration
                     --liftIO $ print morse
                    -- forever $ liftIO (threadDelay 100) >> ((getInputValues One) >>= liftIO . print . getRawADValue)

getRawADValue :: InputValue -> RawADValue
getRawADValue (InputValue _ _ _ _ _ x _ _ _) = x

gatherData :: [RawADValue] -> NXT [RawADValue]
gatherData x
    | all (>900) ((take 200) x) = do y <- getInputValues One
                                     let val = getRawADValue y 
                                     return (val:x)
    | otherwise = do liftIO $ threadDelay 100
                     do y <- getInputValues One
                        let val = getRawADValue y
                        gatherData (val:x)

valuesToDuration :: [RawADValue] -> [Int]
valuesToDuration x = (map sum . group . map (\x -> if x < 900 then 1 else -1)) x

durationToMorse :: [Int] -> [Morse]
durationToMorse x = concat (map durHelper x)
                    where
                      durHelper x
                          | x `div` 24 >= 3 = [Dash]
                          | x `div` 24 >= 1 = [Dot]
                          | otherwise = replicate (abs (x `mod` 24)) Space
