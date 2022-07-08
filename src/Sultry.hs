module Sultry ( eval
              , Expr (EBinOp, EInt, EGet, ESet)
              , Op (Add)
              , VarStore
              ) where

import Data.Map (Map)
import qualified Data.Map as Map

-- | The map in which all the variables are globally stored.
type VarStore = Map.Map

-- | Represents all the expressions SultryLang can handle.
data Expr = EInt        Int             -- | The base integer type
          | EFloat      Float           -- | The base float type

          | EBinOp      Op Expr Expr
          | EMonOp      Op Expr

          | EGet        String          -- | Gets a variable value
          | ESet        String Expr     -- | Sets a variable value

data Op = Add

parse' :: String -> Expr
parse' x = (EInt 3)

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
