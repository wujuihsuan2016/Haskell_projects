module Main where

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)

main = do
  loop (M.empty)
loop env = do
  str <- getLine
  if null str then return ()
  else
    let toklst = tokenize str 
        tree = parse toklst 
        (x, env') = evaluate tree env
    in do
      print x
      loop env'
