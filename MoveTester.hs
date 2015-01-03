import Robotics.NXT
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Data.List

power = 80
modes = [MotorOn]
regu = RegulationModeIdle
ratio = 0
tlim = 0

main :: IO ()
main = withNXT "/dev/rfcomm0" logic
        where
          logic = do enableMotorsForxS 10000 [B] 80
                     enableMotorsForxS 250000 [A, C] 80
                     enableMotorsForxS 10000 [B] (-80)
                     

enableMotorsForxS :: Int -> [OutputPort] -> OutputPower -> NXT ()
enableMotorsForxS x y z= do mapM_ enablemotor y
                            liftIO $ threadDelay x
                            mapM_ disablemotor y
                           where
                              disablemotor = \x -> setOutputStateConfirm x z modes regu ratio MotorRunStateIdle tlim
                              enablemotor = \x -> setOutputStateConfirm x z modes regu ratio MotorRunStateRunning tlim
