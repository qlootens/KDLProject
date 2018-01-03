-------------------------------------------------
-------------------------------------------------
-- Jarvis (MBot) Statements
-------------------------------------------------
-------------------------------------------------
module JarvisStmt(
Light(LIGHT1, LIGHT2), Sensor(LINE, DISTANCE),
Direction(FORWARD, BACKWARD, ToLEFT, ToRIGHT)
) where

data Light     = LIGHT1
               | LIGHT2
               deriving (Show)

data Sensor    = LINE
               | DISTANCE
               deriving (Show)

data Direction = FORWARD
               | BACKWARD
               | ToLEFT
               | ToRIGHT
                deriving (Show)
