module Env where

-- Location
type Location = Int
type Dict = [String]
type Stack = [Int]

position :: String -> Dict -> Location
position name dict = fromMaybe $ elemIndex name dict

fetch :: Location -> Stack -> Int
fetch n stack = stack !! n

put :: Location -> Int -> Stack -> Stack 
put 0 x (_:vs) = x:vs
put n x (v:vs) = v:(put (n-1) x vs)

newtype M a = StOut { runStOut :: Stack -> (a, Stack, String))

instance Monad M where
  return x = StOut (\ns -> (x, ns, "")
  e >>= f = StOut (\ns -> let (a1, ns1, s1) = (runStOut e) ns
                             (a2, ns2, s2) = runStOut (f a1) ns1
                         in (b, ns2, s1 ++ s2))

  getfrom :: Location -> M Int 
  getfrom i = StOut (\ns -> (fetch i ns, ns, "")

  write :: Location -> Int -> M ()
  write i v = StOut (\ns -> ((), put i v ns, ""))

  push :: Int -> M ()
  push x = StOut (\ns -> ((), x:ns, ""))

  pop :: M ()
  pop = StOut (\ns@(n:ns') -> ((), ns', ""))

  output :: Show a => a -> M ()
  output v = StOut (\n -> ((), n, show v))
