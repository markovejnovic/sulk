module Main where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Sultry ( eval
              , Expr (EBinOp, EInt, EGet, ESet)
              , Op (Add)
              , VarStore
              , parse
              )

initialVariableTable = Map.empty

main :: IO ()
main = main' initialVariableTable

main' :: VarStore String Int -> IO ()
main' varStore = do
    input <- read'
    let parsed = parse input
    let (result, newStore) = eval varStore parsed

    print $ show result
    main' newStore

read' :: IO String
read' = do
    putStr "> "
    hFlush stdout
    getLine
