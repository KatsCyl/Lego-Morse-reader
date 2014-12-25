module Main where

import Robotics.NXT
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent

main :: IO ()
main = withNXT "/dev/rfcomm0" logic 
        where
          logic = do setInputModeConfirm One SoundDB RawMode
                     forever $ liftIO (threadDelay 100) >> (getRawADValue (getInputValues One) >>= liftIO . print)

getRawADValue :: InputValue -> NXT RawADValue
getRawADValue (InputValue _ _ _ _ _ x _ _ _) = x
