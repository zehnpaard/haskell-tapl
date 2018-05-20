module Repl where

import System.IO

import Types
import Parser
import DeBruijn
import TypeCheck
import Evaluate

loop :: IO () -> IO ()
loop f = f >> loop f

evalToString :: Term -> String
evalToString t = case typeOf [] t of
  Just _  -> show $ eval $ t
  Nothing -> "Type-check failed"

repl :: IO ()
repl = putStrLn "λ calculus evaluator:" >>
       loop (putStr ">>" >> hFlush stdout >>
             getLine >>= (putStrLn . evalToString . convertTerm [] . readExpr))
