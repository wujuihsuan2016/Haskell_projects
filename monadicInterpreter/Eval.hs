module Eval where

import Ast
import Env

eval1 :: Exp -> Dict -> M Int
eval1 exp dict = case exp of
    Const n -> return n
    Var x -> getfrom $ position x dict
    Binop op exp1 exp2 -> do
        v1 <- eval1 exp1 dict;
        v2 <- eval1 exp2 dict;
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

interpret1 :: Com -> Dict -> M ()
interpret1 stmt dict = case stmt of
    Assign name exp -> do 
        v <- eval1 exp dict
        write (position dict name) v
    Seq stmts -> do
        mapM_ (\stmt -> interpret1 stmt dict) stmts
        return ()
    Cond exp stmt1 stmt2 -> do
        value <- eval1 exp dict
        if value == 1
           then interpret1 stmt1 dict
           else interpret1 stmt2 dict
    While exp stmt' -> 
        let loop () = do 
            value <- eval1 exp dict
            if value == 0 
               then return ()
               else do {interpret1 stmt' dict; loop ()}
        in loop ()
    Let name exp stmt' -> do
        value <- eval1 exp dict
        push value
        interpret1 stmt' exp (name:dict)
        pop value
    Print exp ->
        value <- eval1 exp dict
        output value
