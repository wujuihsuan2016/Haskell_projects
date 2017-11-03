module Env where

import Data.Maybe
import Data.List
import Control.Applicative 
import Control.Monad (liftM, ap)

type Location = Int
type Dict = [String]
type Stack = [Integer]

position :: String -> Dict -> Location
position name dict = fromMaybe (-1) $ elemIndex name dict

fetch :: Location -> Stack -> Integer
fetch n stack = stack !! n

put :: Location -> Integer -> Stack -> Stack 
put 0 x (_:vs) = x:vs
put n x (v:vs) = v:(put (n-1) x vs)

newtype M a = StOut (Stack -> (a, Stack, String))

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure x = StOut (\ns -> (x, ns, ""))
  (<*>) = ap 

instance Monad M where
  return = pure
  e >>= f = StOut (\ns -> let (a1, ns1, s1) = (runStOut e) ns
                              (a2, ns2, s2) = runStOut (f a1) ns1
                          in (a2, ns2, s1 ++ s2))

runStOut (StOut f) = f

getfrom :: Location -> M Integer 
getfrom i = StOut (\ns -> (fetch i ns, ns, ""))
  
write :: Location -> Integer -> M ()
write i v = StOut (\ns -> ((), put i v ns, ""))

push :: Integer -> M ()
push x = StOut (\ns -> ((), x:ns, ""))
  
pop :: M ()
pop = StOut (\ns@(n:ns') -> ((), ns', ""))

output :: Show a => a -> M ()
output v = StOut (\n -> ((), n, show v))
