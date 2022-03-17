module Lab4 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Cristian PALMA
-- Números: 208443
----------------------------------------------------

import Prelude
import Data.List

-- EJERCICIO 1.1 --
type Var    = String
type Sym    = String
type Aridad = Int
type Cs     = [Sym]
type Fs     = [(Sym, Aridad)]
type Ps     = [(Sym, Aridad)]
type Vocab  = (Cs, Fs, Ps)

data Term = V Var | C Sym | F Sym [Term]
  deriving (Show, Eq)
data Form = P Sym [Term] | Neg Form | BC BinCon Form Form | Q QT Var Form
  deriving (Show, Eq)
data BinCon = And | Or | Impl | Iff
  deriving (Show, Eq)
data QT = All | Ex
  deriving (Show, Eq)

  
-- EJERCICIO 1.2 --
--a) . g(y, z)
ta :: Term
ta = F "g" [V "y", V "z"]
--b) . g(f(x), a)
tb :: Term
tb = F "g" [ (F "f" [V "x"]), (C "a")]


-- EJERCICIO 1.3 --
--a) (∀x)P(x) ⊃ (∀y)Q(y) ⊃ (∀z)R(z)
fa :: Form
fa = BC Impl (Q All "x" (P "P" [V "x"])) (BC Impl (Q All "y" (P "Q" [V "y"])) (Q All "z" (P "R" [V "z"])))

--b)  (∀x)P(x) ⊃ (∃y)(Q(x, y) ∧ R(g(y, z)))
fb :: Form
fb = BC Impl (Q All "x" (P "P"[V "x"]))(Q Ex "y" (BC And (P "Q"([(V "x"),(V "y")]))(P "R" [ta])))


--c) (R(x, a) ∨ (∀x)Q(g(f(x), a), c)) ∧ ¬(∀z)P(z)
fc :: Form
fc = BC And (BC Or (P "R"[(V "x"),(C "a")])(Q All "x" (P "Q"[(F "g"[(F "f"[V "x"]),(C "a")]),(C "c")]) ))(Neg(Q All "z"(P "P" [V "z"])))


-- EJERCICIO 1.4 --
--a)
occurs :: Var -> Term -> Bool
occurs  v (V x) = v == x
occurs  v (C _) = False
occurs  v (F _ ts) = any (occurs v) ts


--b)
loccurs :: Var -> [Term] -> Bool
loccurs v [] = False
loccurs v (t:ts) = occurs v t  || loccurs v ts

 

--c)
vars :: Term -> [Var]
vars (V x) = [x]
vars (C _) = []
vars (F _ ts) =  nub $concat (map vars ts)



--d)
freevars :: Form -> [Var]
freevars (P _ ts) = nub $concat (map vars ts)
freevars (Neg f) = nub $ freevars f
freevars (BC _ f1 f2) = nub (freevars f1 ++ freevars f2)
freevars (Q _ x f) = nub $ delete x (freevars f)

--e)
free :: Var -> Form -> Bool
free v f = elem v (freevars f)
   
--f)
boundvars :: Form -> [Var]  
boundvars (P _ ts) = []
boundvars (Neg f) = nub $ boundvars f
boundvars (BC _ f1 f2) = nub (boundvars f1 ++ boundvars f2)
boundvars (Q _ x f) =  nub $ x :(boundvars f)
       
--g)                   
unfree :: Var -> Form -> Bool
unfree v f= not (free v f) && elem v (boundvars f)

--h) 
fresh :: Var -> Form -> Bool
fresh v f = not (free v f) && not (unfree v f)  

--i)
vocab :: Form -> Vocab
vocab (P p ts) =  unirTern $ ([],[],[(p, length ts)]) : map auxiliar ts
vocab (Neg f) = vocab f
vocab (BC _ f1 f2) = unirTern [vocab f1, vocab f2]
vocab (Q _ x f1) =  case (vocab f1) of {
            (c,f,p) ->(delete x (c), f, p)

}   

unirTern:: [Vocab] -> Vocab
unirTern [] =([],[],[])
unirTern ((cs1, fs1, ps1):xs)= case (unirTern xs) of {
                             (cs2, fs2, ps2) -> (nub $ cs1 ++ cs2,nub $ fs1 ++ fs2 , nub $ ps1 ++ ps2)

}

auxiliar :: Term -> Vocab
auxiliar (V x) = ([x],[],[])
auxiliar (C x) = ([x],[],[])
auxiliar (F f ts) =  unirTern $ ([],[(f, length ts)],[]) : map auxiliar ts 

