module Eval where

import Ast
import Env

eval :: Exp -> Dict -> M Integer
eval exp dict = case exp of
    Const n -> return n
    Var x -> getfrom $ position x dict
    Binop op exp1 exp2 -> do
        v1 <- eval exp1 dict;
        v2 <- eval exp2 dict;
        return $ case op of
            Plus    -> v1 + v2
            Minus   -> v1 - v2
            Times   -> v1 * v2
            Divide  -> v1 `quot` v2
            Greater -> if v1 >  v2 then 1 else 0
            Less    -> if v1 <  v2 then 1 else 0
            Gequal  -> if v1 >= v2 then 1 else 0
            Lequal  -> if v1 <= v2 then 1 else 0
            Equal   -> if v1 == v2 then 1 else 0
            Unequal -> if v1 /= v2 then 1 else 0

interpret :: Com -> Dict -> M ()
interpret stmt dict = case stmt of
    Assign name exp -> do 
        v <- eval exp dict
        write (position name dict) v
    Seq stmts -> do
        mapM_ (\stmt -> interpret stmt dict) stmts
        return ()
    Cond exp stmt1 stmt2 -> do
        value <- eval exp dict
        if value == 1
           then interpret stmt1 dict
           else interpret stmt2 dict
    While exp stmt' -> 
        let loop () = do 
            value <- eval exp dict
            if value == 0 
               then return ()
               else do {interpret stmt' dict; loop ()}
        in loop ()
    Let name exp stmt' -> do
        value <- eval exp dict
        push value
        interpret stmt' (name:dict)
        pop
    Print exp -> do
        value <- eval exp dict
        output value
