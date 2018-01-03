-------------------------------------------------
-------------------------------------------------
-- Evaluator
-------------------------------------------------
-------------------------------------------------
module Evaluator(
evalExpression, KDLMap
) where

import qualified    Data.Map
import              Expressions

-------------------------------------------------
-- create KDLMap
-------------------------------------------------
type KDLMap = Data.Map.Map String Value

-------------------------------------------------
-- Evaluate Expressions
-------------------------------------------------
evalExpression :: KDLMap -> Exp -> KDLValue
evalExpression kdlmap      (Lit n)  = valueToKDLValue $ evalOperation kdlmap (Lit n)
evalExpression kdlmap (Variable n)  = searchVariable n kdlmap
evalExpression kdlmap (Assign n v)  =
  return $ KDLFunction (\x -> (evalExpression (Data.Map.insert n x kdlmap) v))
evalExpression kdlmap (Assist n v)  =
  return $ KDLFunction (\x -> evalExpression (Data.Map.insert n x kdlmap) v)
evalExpression kdlmap (Apply  a b)  =
  mapM (evalExpression kdlmap) [a, b] >>= \[assis, arg]
        -> evalKDLFunction assis arg

evalExpression kdlmap      expr     = case expr of
  (e :+:  f)   -> valueToKDLValue $ evalOperation kdlmap (e :+:  f)
  (e :*:  f)   -> valueToKDLValue $ evalOperation kdlmap (e :*:  f)
  (e :-:  f)   -> valueToKDLValue $ evalOperation kdlmap (e :-:  f)
  (e :<:  f)   -> valueToKDLValue $ evalOperation kdlmap (e :<:  f)
  (e :>:  f)   -> valueToKDLValue $ evalOperation kdlmap (e :>:  f)
  (e :<=: f)   -> valueToKDLValue $ evalOperation kdlmap (e :<=: f)
  (e :>=: f)   -> valueToKDLValue $ evalOperation kdlmap (e :>=: f)
  (e :==: f)   -> valueToKDLValue $ evalOperation kdlmap (e :==: f)
  (e :/=: f)   -> valueToKDLValue $ evalOperation kdlmap (e :/=: f)
  (e :&&: f)   -> valueToKDLValue $ evalOperation kdlmap (e :&&: f)
  (e :||: f)   -> valueToKDLValue $ evalOperation kdlmap (e :||: f)
  _            -> Prelude.error "Something went wrong in evalExpression."


evalOperation :: KDLMap -> Exp -> Value
evalOperation kdlmap expr = case expr of
  (Lit n)      -> n
  (e :+:  f)   -> intToKLDInt   $ evalInt     kdlmap (e :+:  f)
  (e :*:  f)   -> intToKLDInt   $ evalInt     kdlmap (e :*:  f)
  (e :-:  f)   -> intToKLDInt   $ evalInt     kdlmap (e :-:  f)
  (e :<:  f)   -> boolToKDLBool $ evalIntBool kdlmap (e :<:  f)
  (e :>:  f)   -> boolToKDLBool $ evalIntBool kdlmap (e :>:  f)
  (e :<=: f)   -> boolToKDLBool $ evalIntBool kdlmap (e :<=: f)
  (e :>=: f)   -> boolToKDLBool $ evalIntBool kdlmap (e :>=: f)
  (e :==: f)   -> boolToKDLBool $ evalIntBool kdlmap (e :==: f)
  (e :/=: f)   -> boolToKDLBool $ evalIntBool kdlmap (e :/=: f)
  (e :&&: f)   -> boolToKDLBool $ evalBool    kdlmap (e :&&: f)
  (e :||: f)   -> boolToKDLBool $ evalBool    kdlmap (e :||: f)
  _            -> Prelude.error "Something went wrong in the evalOperation"

-------------------------------------------------
-- Evaluate Expressions Utilities
-------------------------------------------------
-- Evaluate Integer Arithmetic operations
evalInt :: KDLMap -> Exp -> Int
evalInt _ (Lit n)          =  kdlintToInt n
evalInt k (Variable n)     =  kdlintToInt $ searchVariable' n k
evalInt k (e :+: f)        =  evalInt k e + evalInt k f
evalInt k (e :*: f)        =  evalInt k e * evalInt k f
evalInt k (e :-: f)        =  evalInt k e - evalInt k f
evalInt _ _                =  Prelude.error "Unable to perform operation."

-- Evaluate Boolean operations with Integers
evalIntBool :: KDLMap -> Exp -> Bool
evalIntBool k (e :<:   f)         =  evalInt k e <  evalInt k f
evalIntBool k (e :>:   f)         =  evalInt k e >  evalInt k f
evalIntBool k (e :<=:  f)         =  evalInt k e <= evalInt k f
evalIntBool k (e :>=:  f)         =  evalInt k e >= evalInt k f
evalIntBool k (e :/=:  f)         =  evalInt k e /= evalInt k f
evalIntBool k (e :==:  f)         =  evalInt k e == evalInt k f
evalIntBool _ _            =  Prelude.error "Unable to perform operation."

-- Evaluate Boolean operations with Booleans
evalBool :: KDLMap -> Exp -> Bool
evalBool _ (Lit n)         =  kdlboolToBool n
evalBool k (Variable n)    =  kdlboolToBool $ searchVariable' n k
evalBool k (e :&&: f)      =  evalBool k e && evalBool k f
evalBool k (e :||: f)      =  evalBool k e || evalBool k f
evalBool _  _              =  Prelude.error "Unable to perform operation."


-- Evaluate a KDLFunction
evalKDLFunction :: Value -> Value -> KDLValue
evalKDLFunction (KDLFunction k) a = k a
evalKDLFunction a b = Left  $ "Internal error: " ++ show a ++
                              " with argument" ++ show b ++ "."

-------------------------------------------------
-- Utility functions
-------------------------------------------------
kdlintToInt :: Value -> Int
kdlintToInt (KDLInt n)          = n
kdlintToInt _                   = Prelude.error $ "Internal error" ++
                                                " KDLInt to Int"

kdlboolToBool :: Value -> Bool
kdlboolToBool (KDLBool n)       = n
kdlboolToBool _                 = Prelude.error $ "Internal error" ++
                                                " KDLBool to Bool"

intToKLDInt :: Int -> Value
intToKLDInt                     = KDLInt

boolToKDLBool :: Bool -> Value
boolToKDLBool                   = KDLBool

valueToKDLValue :: Value -> KDLValue
valueToKDLValue = return

-- Find a variable in the KDLMap and return a KDLValue
searchVariable :: Name -> KDLMap -> KDLValue
searchVariable x m = case Data.Map.lookup x m of
  Just v  -> return v
  Nothing -> Left ("Unbound variable: " ++ x)

-- Find a variable in the KDLMap and return a Value
searchVariable' :: Name -> KDLMap -> Value
searchVariable' x m = case Data.Map.lookup x m of
  Just v  ->  v
  _       -> Prelude.error "Something went wrong in searchVariable'"
