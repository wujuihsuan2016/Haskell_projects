module Ast where

-- Expressions --
data Exp = Const Integer
         | Var String
         | Binop Op Exp Exp
         deriving Show

-- Operators --
data Op = Plus
        | Minus
        | Times
        | Divide
        | Greater
        | Less
        | Gequal
        | Lequal
        | Equal
        | Unequal
        deriving Show

-- Commands --
data Com = Assign String Exp
         | Seq [Com]
         | Cond Exp Com Com
         | While Exp Com    
         | Let String Exp Com
         | Print Exp
         deriving Show
