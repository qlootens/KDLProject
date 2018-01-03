-------------------------------------------------
-------------------------------------------------
-- Parse Data (Strings, Characters, Numbers, etc)
-------------------------------------------------
-------------------------------------------------
module DataParser (
char, spot, token, notToken, bracket, bracket', roundBracket, whitespace,
addWhitespace, whiteToken, match, whiteMatch, whiteIdentifier, parseString,
parseLine, checkLegalIdentifier, parseNat, parseNeg, parseDouble, parseInt
) where

import          Parser
import          Data.Char
import          Control.Monad

-------------------------------------------------
-- Parse Strings and Characters
-------------------------------------------------
-- Parse one character
char :: Parser Char
char = Parser f
  where
    f [] = []
    f (c:s) = [(c,s)]

-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do
  c <- char
  guard (p c)
  return c

-- Match a given Character
token :: Char -> Parser Char
token c = spot (== c)

-- Match a Character not equal to a given Character
notToken :: Char -> Parser Char
notToken c = spot (/= c)

-- Match something between brackets
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket o p c = o >> p >>= \x -> c >> return x

-- Match something between brackets, last parser's value is returned
bracket' :: Parser a -> Parser b -> Parser c -> Parser c
bracket' o c p = bracket o p c

-- Match something between round brackets
roundBracket :: Parser a -> Parser a
roundBracket p = bracket (whiteToken '(') p (whiteToken ')')

-- Parse as much whitespace as possible
whitespace :: Parser String
whitespace = star $ spot isSpace

-- Change a parser so it parses optional whitespace to the right
addWhitespace :: Parser a -> Parser a
addWhitespace p = p >>= \m -> whitespace >> return m

-- Parse a token with optional whitespace to the right
whiteToken :: Char -> Parser Char
whiteToken t = addWhitespace (token t)

-- Match a given String
match :: String -> Parser String
match = mapM token

-- Match a given string with optional whitespace to the right
whiteMatch :: String -> Parser String
whiteMatch s = addWhitespace (match s)

-- Match an identifier with optional whitespace to the right
whiteIdentifier :: Parser String
whiteIdentifier = addWhitespace parseString

-- Match an identifier (starts with a lowercase character and contains
-- only alphanum characters).
parseString :: Parser String
parseString = do
  x   <- spot isLower
  xs  <- star $ spot isAlphaNum
  let name = x:xs
  guard $ checkLegalIdentifier name
  return name

-- Parse until a newline character is matched
parseLine :: Parser String
parseLine = do
   c <- char;
   if c == '\n'
     then whitespace >> return [c]
     else parseLine >>= \s -> return (c:s)

-- Check if given String matches KDL Language material
checkLegalIdentifier :: String -> Bool
checkLegalIdentifier name =
  let identies = ["WAAR", "VALS", "laat", "ALS", "ANDERS", "TERWIJL"]
  in  name `notElem` identies

-------------------------------------------------
-- Parse Numbers
-------------------------------------------------
-- Match a natural number
parseNat :: Parser Int
parseNat = do
  s <- plus (spot isDigit)
  return (read s)

-- Match a negative number
parseNeg :: Parser Int
parseNeg = do
  _ <- token '-'
  n <- parseNat
  return (-n)

-- Match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg

-- Match a Double
parseDouble :: Parser Double
parseDouble = do
  n <- parseInt
  _ <- token '.'
  c <- parseNat
  return $ read (show n ++ "." ++ show c)
