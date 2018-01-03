-------------------------------------------------
-------------------------------------------------
-- Run KDL Language
-------------------------------------------------
-------------------------------------------------
module RunKDL(
run
) where

import              Expressions
import qualified    Data.Map
import              Statements
import              Control.Monad.Trans.State
import              Control.Monad.Trans.Class
import              Jarvis
import              Evaluator
import              MBot
import              JarvisStmt
import              System.HIDAPI

-------------------------------------------------
-- Run KDL
-------------------------------------------------
run :: Statement -> StateT KDLMap IO()

-------------------------------------------------
-- Run JARVIS Statements
-------------------------------------------------
run (GO dir) =
  lift $ do jar <- openMBot
            moveJarvis dir jar
            closeMBot jar
              where moveJarvis :: Direction -> Device -> IO()
                    moveJarvis FORWARD  d   = goForward   d
                    moveJarvis BACKWARD d   = goBackward  d
                    moveJarvis ToLEFT   d   = goLeft      d
                    moveJarvis ToRIGHT  d   = goRight     d

run (SETLIGHT light ex1 ex2 ex3) = do
  kdlm      <- get
  jarvis    <- getJarvis
  let   values = mapM (evalExpression kdlm) [ex1, ex2, ex3]
  case  values of
    Right [KDLInt r, KDLInt g, KDLInt b] -> lift $ setLight light r g b jarvis
    Right _ -> raiseError "Invalid types in light-statement"
    Left  s -> raiseError s
  where setLight :: Light -> Int -> Int -> Int -> Device -> IO ()
        setLight LIGHT1 = leftLight
        setLight LIGHT2 = rightLight

run STOP                          = do
                    jarvis  <- getJarvis
                    lift $ stopMotor jarvis

run (READ LINE n)                 = do
                    jarvis  <- getJarvis
                    kdlm    <- get
                    line    <- lift $ getLineData jarvis
                    put $ Data.Map.insert n (KDLInt line) kdlm

run (READ DISTANCE n)             = do
                    jarvis  <- getJarvis
                    kdlm    <- get
                    dist    <- lift $ getDistance jarvis
                    put $ Data.Map.insert n (KDLInt dist) kdlm

-------------------------------------------------
-- Run KDL Statements
-------------------------------------------------
run (SEQUENCE statements)         = sequence_ $ fmap run statements

run NOTHING                       = skip

run (COMMENT _)                   = run NOTHING

run (n := expression)             = do
        kdlm <- get
        let value = evalExpression kdlm expression in
               case value of
                  Right a -> put $ Data.Map.insert n a kdlm
                  Left  s -> raiseError s

run (IF expression statement)     = run $ IFELSE expression statement NOTHING

run (IFELSE expression ifStatement elseStatement) = do
        kdlm <- get
        let   predicate = evalExpression kdlm expression
        case  predicate of
          Right (KDLBool True)  -> run ifStatement
          Right (KDLBool False) -> run elseStatement
          Left  s               -> raiseError s
          _                     -> raiseError "Invalid type in if-statement"

run (WHILE expression statement)  = do
  kdlm <- get
  let   predicate = evalExpression kdlm expression
  case  predicate of
    Right (KDLBool True)   -> run statement >> run (WHILE expression statement)
    Right (KDLBool False)  -> skip
    Left  s                -> raiseError s
    Right a                -> raiseError ("Invalid type in while-statement: "
                              ++ show a)

run (PRINT expression)            = do
          kdlm <- get
          let value = evalExpression kdlm expression
          lift $ putStrLn (case value of
            Right a -> show a
            Left  s -> s)

run (EXEC statement)              = do
          startKDLM   <- get
          run statement
          endKDLM     <- get
          let changed = Data.Map.intersection endKDLM startKDLM
          let globals = Data.Map.filterWithKey (\k _ -> isJARVIS k) endKDLM
          let newKDLM = changed `Data.Map.union` startKDLM `Data.Map.union` globals
          put newKDLM

-------------------------------------------------
-- Utilities
-------------------------------------------------
getJarvis :: StateT KDLMap IO Device
getJarvis = do
  kdlm <- get
  case Data.Map.lookup "_JARVIS" kdlm of
    Just (JARVIS m) -> return m
    _               -> do
      jar <- lift openMBot
      put $ Data.Map.insert "_JARVIS" (JARVIS jar) kdlm
      return jar

--Show an error message
raiseError :: String -> StateT KDLMap IO ()
raiseError = lift . fail . ("RUNTIME ERROR: " ++)

--Do nothing
skip :: StateT KDLMap IO ()
skip =  lift $ return ()

--Test if a variable name represents a global variable
isJARVIS :: String -> Bool
isJARVIS s = head s == '_'
