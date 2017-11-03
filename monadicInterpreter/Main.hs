module Main where

import Lexer
import Ast
import Env
import Eval
import Parser

evalProgram :: String -> String
evalProgram s = 
  case parseProgram s of
    Left msg -> show msg
    Right res -> 
      case runStOut (interpret res []) [] of
        (_, _, ans) -> ans
                               
main :: IO ()
main = do
  putStrLn "Enter the filename: "
  filename <- getLine 
  s <- readFile filename
  putStrLn (evalProgram s) 
