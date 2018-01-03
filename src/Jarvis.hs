-------------------------------------------------
-------------------------------------------------
-- JARVIS Engine Commands
-------------------------------------------------
-------------------------------------------------
module Jarvis (
getDistance, getLineData, leftLight, rightLight, goForward, goBackward,
turnRight, turnLeft, stopMotor
) where

import qualified    MBot            as JARVIS
import              System.HIDAPI
import              Control.Concurrent

-------------------------------------------------
-- JARVIS Sensors
-------------------------------------------------
-- Get the distance using the ultrasonic sensor
getDistance :: Device -> IO Int
getDistance d = do
  f <- JARVIS.readUltraSonic d;
  return $ truncate f

-- Read the line linesensor
getLineData :: Device -> IO Int
getLineData d = do
   status <- JARVIS.readLineFollower d
   case status of
     JARVIS.LEFTB  -> return 1
     JARVIS.RIGHTB -> return 2
     JARVIS.BOTHB  -> return 3
     JARVIS.BOTHW  -> return 4

-------------------------------------------------
-- JARVIS Lights
-------------------------------------------------
-- Set the left led color using rgb notation
leftLight :: Int -> Int -> Int -> Device -> IO ()
leftLight r g b d  = do
  threadDelay 500000;
  setLed 1 r g b d;

-- Set the right led color using rgb notation
rightLight :: Int -> Int -> Int -> Device -> IO ()
rightLight r g b d = do
  threadDelay 500000;
  setLed 2 r g b d;

-- Set the led (index) color using rgb notation
setLed :: Int -> Int -> Int -> Int -> Device -> IO ()
setLed index r g b d = JARVIS.sendCommand d $ JARVIS.setRGB index r g b

-------------------------------------------------
-- JARVIS Driving Skills
-------------------------------------------------
-- Move the motor forward at the given speed
goForward :: Device -> IO ()
goForward d = do
  threadDelay 1000000;
  JARVIS.goBackwards d;

-- Move the motor backward at the given speed
goBackward :: Device -> IO ()
goBackward d = do
  threadDelay 1000000;
  JARVIS.goAhead d;

-- Turn right at the given speed
turnRight :: Device -> IO ()
turnRight d = do
  threadDelay 1000000;
  JARVIS.goRight d;
  threadDelay 3000000;
  JARVIS.stop d;

-- Turn left at the given speed
turnLeft :: Device -> IO ()
turnLeft d = do
  threadDelay 1000000;
  JARVIS.goLeft d;
  threadDelay 3000000;
  JARVIS.stop d;

-- Stop the motor
stopMotor :: Device -> IO ()
stopMotor d = do
  threadDelay 500000;
  JARVIS.stop d
