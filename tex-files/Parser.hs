-------------------------------------------------
-------------------------------------------------
-- Monad Parser
-------------------------------------------------
-------------------------------------------------
module Parser where

import              Control.Applicative
import              Control.Monad

-------------------------------------------------
-- Initialize Parser
-------------------------------------------------
-- The type of parsers, create a new tpe
newtype Parser a = Parser (String -> [(a, String)])

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
  where
    one [] = error "no parse"
    one [x] = x
    one xs | length xs > 1 = error "ambiguous parse"
    one _  = error "something want wrong in the parse"

-- Add Functor and Applicative Instances for making Parser a Monad
instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
  (<*>) = ap

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus m n = Parser (\s -> apply m s ++ apply n s)

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
  return = pure
  m >>= k = Parser (\s ->
                [ (y, u) |
                (x, t) <- apply m s,
                (y, u) <- apply (k x) t ])

-------------------------------------------------
-- Utilities
-------------------------------------------------
-- Match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []

-- Match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = do{
  x <- p;
  xs <- star p;
  return (x:xs);
}

-- parse a sequence of n time parser p seperated by parser sep
sepByN :: Int -> Parser a -> Parser b -> Parser [a]
sepByN n p sep = do
                x   <- p
                xs  <- replicateM (n - 1) (sep >> p)
                return (x:xs)
