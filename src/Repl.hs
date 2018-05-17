module Repl where

import System.IO

import Types
import Parser
import DeBruijn
import Evaluate

loop :: IO () -> IO ()
loop f = f >> loop f

repl :: IO ()
repl = putStrLn "λ calculus evaluator:" >>
       loop (putStr ">>" >> hFlush stdout >>
             getLine >>= (putStrLn . show . eval . convertTerm [] . readExpr))
