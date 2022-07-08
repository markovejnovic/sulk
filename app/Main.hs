module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Sultry ( eval
              , Expr (EBinOp, EInt, EGet, ESet)
              , Op (Add)
              , VarStore
              )

initialVariableTable = Map.empty

main :: IO ()
main = main' initialVariableTable

main' :: VarStore String Int -> IO ()
main' varStore = do
    let (result, newStore) = eval varStore (ESet "myVar" (EBinOp Add (EGet "myVar") (EInt 1)))

    let (veryNewResult, veryNewStore) = eval newStore (EBinOp Add (EGet "myVar") (EInt 2))
    print veryNewResult
    main' veryNewStore

read' :: IO String
read' = do
    putStr "> "
    hFlush stdout
    getLine
