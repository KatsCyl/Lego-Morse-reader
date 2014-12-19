module Main where

import Robotics.NXT
import Control.Monad.IO.Class
import Control.Concurrent

main :: IO ()
main = withNXT "/dev/rfcomm1" logic 
        where
          logic = do setOutputStateConfirm A 100 [MotorOn] RegulationModeIdle 0 MotorRunStateRunning 0
                     liftIO $ threadDelay 10000000
