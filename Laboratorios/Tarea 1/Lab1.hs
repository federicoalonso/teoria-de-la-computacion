module Lab1 where

-- Federico Alonso 182999

type B = (String,([String],E))

data E = X String
    | C String
    | Lam [String] E
    | App E [E]
    | Case E [B]
    | Rec [(String,E)]

type Sigma = [(String,E)]

buscar :: String -> Sigma -> E
buscar x [] = error "No se encuentra el valor."
buscar x (m:ms)
    | ((fst m) == x) = snd m 
    | otherwise = buscar x ms
-- buscar "x" [("x",X "x")]


sust :: E -> Sigma -> E
sust (X x) s = buscar x s
-- sust (X x) sig = lookup x sig

-- sust (X "x") [("x", C "0")]