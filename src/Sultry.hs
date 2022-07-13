module Sultry ( eval
              , Expr (EBinOp, EInt, EGet, ESet)
              , Op (Add)
              , VarStore
              , parse
              ) where

-- Sulry is a simple language used for easy calculations.
-- This language delineates between statements and expressions.
-- An expression is a lexical element that can be simplified to an evaluated value.
-- A statement is a collection of expressions.
-- A statement can be evaluated to a single value.
--
-- Therefore, the following is valid sultry:
--
--     (2 + 4) + 3
-- 
-- In order to modify inputs, sultry uses literal-modifiers

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read

-- | The map in which all the variables are globally stored.
type VarStore = Map.Map

-- | A statement is merely a string
type Statement = String

-- | A token is also a string.
data Token = TNum     Num
           | TLParen
           | TRParen
           | TAssign
           | TNamed   Name

-- | Represents all the expressions SultryLang can handle.
data Expr = EInt        Int             -- | The base integer type
          | EFloat      Float           -- | The base float type

          | EBinOp      Op Expr Expr
          | EMonOp      Op Expr

          | EGet        String          -- | Gets a variable value
          | ESet        String Expr     -- | Sets a variable value

data Op = Add

-- | Function for evaluating a SultryLang expression.
eval :: VarStore String Int -> Expr -> (Int, VarStore String Int)
eval vars (EInt i) = (i, vars)
eval vars (EBinOp op expr1 expr2) = (evalOp op eval1 eval2, vars2)
    where
        (eval1, vars1) = eval vars expr1
        (eval2, vars2) = eval vars1 expr2

eval vars (EGet key) = case Map.lookup key vars of
                         Nothing -> (0, vars)
                         Just x -> (x, vars)

eval vars (ESet key value) = (val, Map.insert key val vars')
    where (val, vars') = eval vars value

-- | An implementation for the evaluation of all the known operations.
evalOp :: Op -> Int -> Int -> Int
evalOp Add x y = x + y

tokenMatch :: String -> Token
tokenMatch x
  | x == "(" = TLParen
  | x == ")" = TRParen
  | x == "<-" = TAssign
  | isJust(readMaybe x :: Maybe Num) = (TNum read x :: Num)
  | otherwise = (TNamed x)

tokenize :: String -> [Token]
tokenize x = map tokenMatch words x

-- | Parses a token into an expresion
parseVal' :: Token -> Expr
parseVal' tkn
  | isJust(readMaybe tkn :: Maybe Int) = (EInt (read tkn :: Int))
  | isJust(readMaybe tkn :: Maybe Float) = (EFloat (read tkn :: Float))

parse' :: [Token] -> Expr
parse' tkns = 

parse :: String -> Expr
parse = parse' . tokenizeStmt
