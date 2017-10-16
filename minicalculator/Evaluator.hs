module Evaluator (evaluate) where

import Lexer
import Parser
import qualified Data.Map as M

type VarTbl = M.Map String Double

evaluate :: ParseTree -> VarTbl -> (Double, VarTbl)
evaluate (Num x) env = (x, env)

evaluate (SumDiff op t1 t2) env = 
  let (x1, env') = evaluate t1 env
      (x2, env'') = evaluate t2 env'
  in 
  case op of 
    Plus -> (x1 + x2, env'')
    Minus -> (x1 - x2, env'')
    _ -> error "TypeError"

evaluate (ProdDiv op t1 t2) env =
  let (x1, env') = evaluate t1 env 
      (x2, env'') = evaluate t2 env'
  in
  case op of 
    Times -> (x1 * x2, env'')
    Div -> (x1 / x2, env'')
    _ -> error "TypeError"

evaluate (Unop op t) env =
  let (x, env') = evaluate t env
  in
  case op of 
    Plus -> (x, env')
    Minus -> (-x, env')
    _ -> error "TypeError"

evaluate (Var str) env = (lookUp str env, env)

evaluate (Assign str t) env = 
  let (x, env') = evaluate t env 
      env'' = insert str x env'
  in (x, env'')

lookUp :: String -> VarTbl -> Double
lookUp str env =
  case M.lookup str env of
    Just x -> x
    Nothing -> error $ "Undefined variable" ++ str 

insert :: String -> Double -> VarTbl -> VarTbl
insert str x env = 
  M.insert str x env  
