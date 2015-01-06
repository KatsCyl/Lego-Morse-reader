module Sound where

import Robotics.NXT
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent
import Data.List
import Data.List.Split
import Data.Maybe
import Move

threshold = 900 -- Threshold
samplerate = 10 -- In microseconds, takes sample every n micros 
cuttime =132 -- Number of silent samples to stop sampling
cutthreshold = 900

data Morse = Dot | Dash | BSpace | LSpace | WSpace deriving (Show, Eq) -- Datatype to represent morse code
type MorseCode = [Morse]

dict :: [(MorseCode, Char)] -- Dictionary for morsecode -> alphabet conversion, obsolete
dict =
    [([Dot, Dash]             , 'A'),
     ([Dash, Dot, Dot, Dot]   , 'B'),
     ([Dash, Dot, Dash, Dot]  , 'C'),
     ([Dash, Dot, Dot]        , 'D'),
     ([Dot]                   , 'E'),
     ([Dot, Dot, Dash, Dot]   , 'F'),
     ([Dash, Dash, Dot]       , 'G'),
     ([Dot, Dot, Dot, Dot]    , 'H'),
     ([Dot, Dot]              , 'I'),
     ([Dot, Dash, Dash, Dash] , 'J'),
     ([Dash, Dot, Dash]       , 'K'),
     ([Dot, Dash, Dot, Dot]   , 'L'),
     ([Dash, Dash]            , 'M'),
     ([Dash, Dot]             , 'N'),
     ([Dash,Dash,Dash]        , 'O'),
     ([Dot,Dash,Dash,Dot]     , 'P'),
     ([Dash, Dash, Dot, Dash] , 'Q'),
     ([Dot, Dash, Dot]        , 'R'),
     ([Dot, Dot, Dot]         , 'S'),
     ([Dash]                  , 'T'),
     ([Dot, Dot, Dash, Dash]  , 'U'),
     ([Dot, Dot, Dot, Dash]   , 'V'),
     ([Dot, Dash, Dash]       , 'W'),
     ([Dash, Dot, Dot, Dash]  , 'X'),
     ([Dash, Dot, Dash, Dash] , 'Y'),
     ([Dash, Dash, Dot, Dot]  , 'Z'),
     ([WSpace]                , ' ')
     ]

getRawADValue :: InputValue -> RawADValue
getRawADValue (InputValue _ _ _ _ _ x _ _ _) = x

-- gatherData gathers input data as long as the last n(cuttime) values are non silent.
gatherData :: [RawADValue] -> NXT [RawADValue] 
gatherData x
    | all (>cutthreshold) (take cuttime x) = do y <- getInputValues One
                                                let val = getRawADValue y 
                                                return (val:x)
    | otherwise = do liftIO $ threadDelay samplerate
                     do y <- getInputValues One
                        let val = getRawADValue y
                        gatherData (val:x)

valuesToDuration :: [RawADValue] -> [Int]
valuesToDuration = map sum . group . map (\x -> if x < threshold then 1 else -1)

durationToMorse :: [Int] -> [Morse]
durationToMorse = concatMap durHelper
                    where
                      durHelper x
                          | x >= 15 = [Dash]
                          | x >= 1 = [Dot]
                          | x <= -45 = [WSpace]
                          | x <= -20 = [LSpace]
                          | x <= 0 = [BSpace]
                          | otherwise = []

decodeLetter :: MorseCode -> Char
decodeLetter morse = fromMaybe ' ' $ lookup morse dict

decodeSentence :: MorseCode -> String
decodeSentence morse = map decodeLetter $ concat $ splitLetters $ splitWords morse

splitLetters :: [MorseCode] -> [[MorseCode]]
splitLetters = map (splitOn [LSpace])

splitWords :: MorseCode -> [MorseCode]
splitWords = split (oneOf [WSpace])

morseToMoveAction :: Morse -> NXT ()
morseToMoveAction morse
                | morse == Dot = drawPoint
                | morse == Dash = drawLine
                | morse == LSpace = replicateM_ 2 moveTick
                | morse == WSpace = replicateM_ 3 moveTick
                | otherwise = moveTick

cleanFromBspaces :: MorseCode -> MorseCode
cleanFromBspaces = filter (/= BSpace)

dropRubbish :: MorseCode -> MorseCode
dropRubbish morse = drop (fromMaybe 0 (elemIndex WSpace morse)) morse
