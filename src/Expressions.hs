-------------------------------------------------
-------------------------------------------------
-- Expressions
-------------------------------------------------
-------------------------------------------------
module Expressions where

import              Parser
import              DataParser
import              Control.Monad
import              System.HIDAPI

-------------------------------------------------
-- Define Values, KDLValues and Expressions
-------------------------------------------------
type Name = String
type KDLValue = Either String Value

data Value = KDLInt       Int
           | KDLBool      Bool
           | KDLFunction  (Value -> KDLValue)
           | KDLString    String
           | JARVIS       Device
           | KDLDouble    Double

instance Show Value where
     show (KDLInt i)      = show i
     show (KDLBool s)     = show s
     show (KDLFunction _) = "(Function)"
     show (JARVIS _)      = "JARVIS"
     show (KDLString s)   = show s
     show (KDLDouble d)   = show d

data Exp = Lit Value
         | Assign Name Exp
         | Variable Name
         | Apply Exp Exp
         | Assist Name Exp
         | Exp :+:  Exp
         | Exp :-:  Exp
         | Exp :*:  Exp
         | Exp :=:  Exp
         | Exp :==: Exp
         | Exp :/=: Exp
         | Exp :>:  Exp
         | Exp :>=: Exp
         | Exp :<:  Exp
         | Exp :<=: Exp
         | Exp :&&: Exp
         | Exp :||: Exp
         deriving (Show)

-------------------------------------------------
-- Parse KDLValues to Expressions
-------------------------------------------------
-- Parse a KDLValue
parseKDLValueIntoExp :: Parser Exp
parseKDLValueIntoExp = parseKDLInt `mplus` parseKDLBool `mplus` parseString'
                       `mplus` parseDouble'

-- Parse a KDLInt Value
parseKDLInt :: Parser Exp
parseKDLInt = fmap (Lit . KDLInt) (addWhitespace parseInt)

-- Parse a KDLBool Value
parseKDLBool :: Parser Exp
parseKDLBool = do
                s <- whiteMatch "WAAR" `mplus` whiteMatch "VALS"
                return $ Lit $ KDLBool (s == "WAAR")

-- Parse a Double value
parseDouble' :: Parser Exp
parseDouble' = (Lit . KDLDouble) <$> addWhitespace DataParser.parseDouble

--Parse a string
parseString' :: Parser Exp
parseString' = bracket' (token '"') (whiteToken '"') $
              (Lit . KDLString) <$> star (notToken '"')

-------------------------------------------------
-- Parse Expressions
-------------------------------------------------
parseExp :: Parser Exp
parseExp = parseLit   `mplus` parseAdd    `mplus` parseMin    `mplus`
           parseMul   `mplus` parseLessS  `mplus` parseLess   `mplus`
           parseGreat `mplus` parseGreatS `mplus` parseOr     `mplus`
           parseAnd   `mplus` parseAssign `mplus` parseEq     `mplus`
           parseNEq   `mplus` parseApply  `mplus` parseVar
            where
    parseLit                = parseKDLValueIntoExp

    parseAdd                = do
                                (d, e) <- parseBinaryOp " + "
                                return (d :+: e)

    parseMin                = do
                                (d, e) <- parseBinaryOp " - "
                                return (d :-: e)

    parseMul                = do
                                (d, e) <- parseBinaryOp " * "
                                return (d :*: e)

    parseLessS              = do
                                (d, e) <- parseBinaryOp " < "
                                return (d :<: e)

    parseLess               = do
                                (d, e) <- parseBinaryOp " <= "
                                return (d :<=: e)

    parseGreat              = do
                                (d, e) <- parseBinaryOp " >= "
                                return (d :>=: e)

    parseGreatS             = do
                                (d, e) <- parseBinaryOp " > "
                                return (d :>: e)

    parseOr                 = do
                                (d, e) <- parseBinaryOp " || "
                                return (d :||: e)

    parseAnd                = do
                                (d, e) <- parseBinaryOp " && "
                                return (d :&&: e)

    parseAssign             = do
                                n <- parseName
                                _ <- match " = "
                                v <- parseExp
                                return (n :=: v)

    parseVar                = Variable <$> whiteIdentifier

    parseEq                 = do
                                (d, e) <- parseBinaryOp " == "
                                return (d :==: e)

    parseNEq                = do
                                (d, e) <- parseBinaryOp " != "
                                return (d :/=: e)

    parseApply              = roundBracket $ Apply
                                <$> parseExp
                                <*> roundBracket parseExp

-------------------------------------------------
-- Utility Parse functions
-------------------------------------------------
-- Utility function to make it cleaner
parseBinaryOp :: String -> Parser (Exp, Exp)
parseBinaryOp ope = do
                  _ <- whiteMatch "( "
                  f <- parseExp
                  _ <- match ope
                  e <- parseExp
                  _ <- whiteMatch " )"
                  return (f, e)

-- Parse an expression surrounded by round brackets
parseExpBracket :: Parser Exp
parseExpBracket = roundBracket parseExp

parseExpBracketN :: Int -> Parser [Exp]
parseExpBracketN n = roundBracket $ sepByN n parseExp (whiteToken ',')

-- Parse a variable name
parseName :: Parser Exp
parseName = fmap Variable whiteIdentifier
