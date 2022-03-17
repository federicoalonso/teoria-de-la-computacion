module ExprArit where

--SINTAXIS ABSTRACTA
data E = L Integer | E :+ E | E :* E deriving (Show)

ocho :: Integer
ocho = 8

ochoE :: E
ochoE = L 8

e :: Integer
e = 2+3*5

ee :: E
ee = L 2 :+ (L 3 :* L 5)

eval :: E -> Integer
eval (L x) = x
eval (e1 :+ e2) = (eval e1) + (eval e2)
eval (e1 :* e2) = (eval e1) * (eval e2)