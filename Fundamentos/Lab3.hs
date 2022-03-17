module Lab3 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Cristian Palma
-- Números: 208443
----------------------------------------------------

import Prelude
import Data.List

-- Formalización del lenguaje y otros elementos
type Var = String
type I = [(Var, Bool)]
type Razonamiento = ([L], L)

data L = V Var 
       | Neg L 
       | And L L 
       | Or  L L 
       | Imp L L
       | Iff L L
       deriving (Show, Eq)
   
data Clase = Tau | Contra | Cont | Sat | Fal   
   

-- EJERCICIO --
--1)
bienDefinida :: I -> Bool
bienDefinida [] = True
bienDefinida ((a,b):xs) = not(elem (a, not b)(xs)) && bienDefinida xs

--2)
tableaux :: [L] -> [I]
tableaux ls = nub $tableauxiliar ls []


tableauxiliar :: [L] -> I -> [I]
tableauxiliar [] i = if bienDefinida i then  [nub i] else []
tableauxiliar (l:ls) i = case l of {

       V v       -> tableauxiliar ls (i ++ [(v, True)]);
       Neg (V v) -> tableauxiliar ls (i ++ [(v, False)]);
       And l1 l2 -> tableauxiliar (l1:l2:ls) i;
       Or l1 l2  -> tableauxiliar (l1:ls) i ++ tableauxiliar (l2:ls) i;
       Imp l1 l2 -> tableauxiliar (Neg (l1):ls) i ++ tableauxiliar (l2:ls) i;
       Iff l1 l2 -> tableauxiliar (l1:l2:ls) i ++ tableauxiliar ((Neg l1):(Neg l2):ls) i;
       Neg (Neg l1) -> tableauxiliar (l1:ls) i;
       Neg(And l1 l2) -> tableauxiliar ((Neg l1):ls) i ++ tableauxiliar ((Neg l2):ls) i;
       Neg(Or l1 l2) -> tableauxiliar ((Neg l1):(Neg l2):ls) i;
       Neg (Imp l1 l2) -> tableauxiliar (l1:(Neg l2):ls) i;
       Neg (Iff l1 l2) -> tableauxiliar (l1:(Neg l2):ls) i ++ tableauxiliar ((Neg l1):l2:ls) i;
}

--3)
validez :: Razonamiento -> (Bool, [I])
validez (hp, c) = (tableaux (hp ++ [Neg c]) == [], tableaux (hp ++ [Neg c]))


--4)
-- es Tau    me fijo si el tableaux de Neg l es vacio
-- Es Contra me fijo si Neg l es tautologia
-- es Cont   me fijo que l no sea tautologia y que l no sea contradiccion
-- es Sat    me fijo que l no sea contradiccion
-- es Fal    me dijo que l no sea tautologia


es :: L -> Clase -> Bool
es l Tau = (tableaux [Neg l]) == []
es l Contra = (tableaux [l]) == []
es l Cont = not (es l Tau)  && not (es l Contra)
es l Sat = not (es l Contra)
es l Fal = not (es l Tau)