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
                     liftIO $ threadDelay 1000
                     initInVals <- replicateM 20 ((liftIO $ threadDelay 100) >> getInputValues One)
                     let initRawVals = map getRawADValue initInVals
                     values <- gatherData initRawVals
                     let duration = valuesToDuration values
                         morse = durationToMorse duration
                     liftIO $ print values
                     liftIO $ print duration
                     liftIO $ print morse
                    -- forever $ liftIO (threadDelay 100) >> ((getInputValues One) >>= liftIO . print . getRawADValue)

getRawADValue :: InputValue -> RawADValue
getRawADValue (InputValue _ _ _ _ _ x _ _ _) = x

gatherData :: [RawADValue] -> NXT [RawADValue]
gatherData x
    | all (>500) ((take 20) x) = do y <- getInputValues One
                                    let val = getRawADValue y 
                                    return (val:x)
    | otherwise = do liftIO $ threadDelay 100
                     do y <- getInputValues One
                        let val = getRawADValue y
                        gatherData (val:x)

valuesToDuration :: [RawADValue] -> [Int]
valuesToDuration x = (map sum . group . map (\x -> if x < 500 then 1 else -1)) x

durationToMorse :: [Int] -> [Morse]
durationToMorse x = concat (map durHelper x)
                    where
                      durHelper x
                          | 24 `mod` x >= 3 = [Dash]
                          | 24 `mod` x >= 1 = [Dot]
                          | otherwise = replicate (abs (24 `mod` x)) Space
