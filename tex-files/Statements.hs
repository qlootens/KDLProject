-------------------------------------------------
-------------------------------------------------
-- Parse Statements
-------------------------------------------------
-------------------------------------------------
module Statements where

import              Expressions
import              Parser
import              Control.Monad
import              DataParser
import              JarvisStmt

-------------------------------------------------
-- Create Statements
-------------------------------------------------
data Statement = EXEC Statement
               | IF Exp Statement
               | IFELSE Exp Statement Statement
               | WHILE Exp Statement
               | SEQUENCE [Statement]
               | Name :=  Exp --Variables
               | COMMENT String
               | NOTHING
               | GO Direction
               | SETLIGHT Light Exp Exp Exp
               | STOP
               | READ Sensor Name
               | PRINT Exp
                 deriving (Show)

-------------------------------------------------
-- Parse Statements
-------------------------------------------------
--Parse a KDL Program
parseKDL :: Parser Statement
parseKDL = whitespace >> parseSequence

-- Parse a SEQUENCE of Statements
parseSequence :: Parser Statement
parseSequence   = fmap SEQUENCE (star parseStatement)

-------------------------------------------------
-- Parse Basic Statements
-------------------------------------------------
parseStatement :: Parser Statement
parseStatement = parseAssign   `mplus` parseEXEC   `mplus` parseIf      `mplus`
                 parseIfElse   `mplus` parseWhile  `mplus` parseComm    `mplus`
                 parseWait     `mplus` parseLight  `mplus` parseJarvis  `mplus`
                 parseSensors  `mplus` parsePrint

                  where
      -- Parse a block, a sequence of Statements, wrapped in '{' '}'
      parseEXEC       =
        fmap EXEC (bracket (whiteToken '{') parseSequence (whiteToken '}'))

      -- Parse an IF Statement
      parseIf         =
        IF <$> (whiteMatch "ALS" >> parseExpBracket) <*> parseEXEC

      -- Parse an IF ELSE Statement
      parseIfElse     = do
                  (IF expr ifStmt)  <- parseIf
                  _                 <- whiteMatch "ANDERS"
                  elseStmt          <- parseEXEC
                  return $ IFELSE expr ifStmt elseStmt

      -- Parse a WHILE loop
      parseWhile      =
        WHILE <$> (whiteMatch "TERWIJL" >> parseExpBracket) <*> parseEXEC

      -- Parse a commentline
      parseComm       = COMMENT <$> (whiteMatch "***" >> parseLine)

      -- Parse a variable declaration
      parseAssign     =
        uncurry (:=) <$> (whiteMatch "laat" >> assignThis)
        >>= \s -> endChar >> return s
        where
          assignThis = makeT <$> whiteIdentifier
                             <*> (whiteToken '=' >> parseExp)

      -- Parse a PRINT statement
      parsePrint      = PRINT <$> (whiteMatch "PRINT" >> parseExpBracket)
                              >>= \s -> endChar >> return s

-------------------------------------------------
-- Parse Jarvis Statements
-------------------------------------------------
parseJarvis :: Parser Statement
parseJarvis   =  parseDir "VOORUIT"    FORWARD    `mplus`
                 parseDir "ACHTERUIT"  BACKWARD   `mplus`
                 parseDir "LINKS"      ToLEFT     `mplus`
                 parseDir "RECHTS"     ToRIGHT


parseLight :: Parser Statement
parseLight =      parseL "LICHT1" LIGHT1 `mplus`
                  parseL "LICHT2" LIGHT2

parseWait :: Parser Statement
parseWait  =      do
                _ <- whiteMatch "WACHT"
                return STOP >>= \s -> endChar >> return s

parseSensors :: Parser Statement
parseSensors = parseSensor "JARVIS_LIJN" LINE
               `mplus`
               parseSensor "JARVIS_AFSTAND" DISTANCE

-------------------------------------------------
-- Utility functions
-------------------------------------------------
endChar :: Parser Char
endChar = addWhitespace (token ';')

makeT :: a -> b -> (a, b)
makeT a b = (a, b)

parseL :: Name -> Light -> Parser Statement
parseL name light = do
                _         <- whiteMatch name
                [r, g, b] <- parseExpBracketN 3
                return (SETLIGHT light r g b) >>= \s -> endChar >> return s

parseSensor :: Name -> Sensor -> Parser Statement
parseSensor n s = do
        name  <- whiteMatch "laat"  >> whiteIdentifier
        _     <- whiteToken '='     >> whiteMatch n
        return (READ s name) >>= \l -> endChar >> return l

parseDir :: Name -> Direction -> Parser Statement
parseDir name dir = do
  _           <- whiteMatch name
  return (GO dir) >>= \s -> endChar >> return s
