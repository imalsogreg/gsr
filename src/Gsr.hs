module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Hardware.Arduino
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Sequence as S
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Foldable as F

freq :: Float
freq = 30

winDuration :: Float
winDuration = 10

readPin :: Pin
readPin = analog 0

arduinoInit :: Arduino ()
arduinoInit = do
  setPinMode readPin ANALOG

arduinoEnqueue :: TVar (S.Seq Float) -> Arduino ()
arduinoEnqueue s = forever $ do
  v <- analogRead readPin
  liftIO . atomically $ modifyTVar s (S.drop 1 . (S.|> fromIntegral v))
  delay (floor $ 1000 / freq)
  
main :: IO ()
main = do
  gsrData <- newTVarIO (S.replicate (floor $ winDuration * freq) (0 :: Float))
  enqueAsync <- async $ withArduino True "/dev/ttyACM0" $ do
    arduinoInit
    (arduinoEnqueue gsrData)
  animateFixedIO
    (InWindow "GSR" (500,300) (10,10))
    (makeColor 0.5 0.5 0.5 1)
    (myPic gsrData)
  
myPic :: TVar (S.Seq Float) -> Float -> IO Picture
myPic s t = do
  seq <- atomically $ readTVar s
  return $ Line $ zip [-150..150] (map ((100 `subtract`).(*5)) $ F.toList seq)
