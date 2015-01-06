module Move where

import Robotics.NXT
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Data.List

modes = [MotorOn, Brake]
regu = RegulationModeIdle
ratio = 0
tlim = 0
sec = 1000000
lowerpentime = sec `div` 4
liftpentime = sec `div` 4
moveticktime = sec `div` 15
lowerpenforce = 60
liftpenforce = -65
moveforce = 60

drawPoint :: NXT ()
drawPoint   = do enableMotorsFor lowerpentime [(B, lowerpenforce)] --Lower pen
                 enableMotorsFor liftpentime [(B, liftpenforce)] --Lift pen

drawLine :: NXT ()
drawLine    = do enableMotorsFor lowerpentime [(B, lowerpenforce)]
                 moveTick
                 enableMotorsFor liftpentime [(B, liftpenforce)]

moveTick :: NXT ()
moveTick    = enableMotorsFor moveticktime [(A, moveforce), (C, moveforce)]

enableMotorsFor :: Int -> [(OutputPort, OutputPower)] -> NXT ()
enableMotorsFor time pp           = do mapM_ enablemotors pp
                                       liftIO $ threadDelay time
                                       mapM_ disablemotors pp
                                       liftIO $ threadDelay sec
                                    where
                                      enablemotors (x,y) = setOutputStateConfirm x y modes regu ratio MotorRunStateRunning tlim
                                      disablemotors (x,y) = setOutputStateConfirm x y modes regu ratio MotorRunStateIdle tlim

parseRotationCount :: OutputState -> RotationCount
parseRotationCount (OutputState _ _ _ _ _ _ _ _ _ x) = x
