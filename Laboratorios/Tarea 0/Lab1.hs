module Lab1 where

-- Federico Alonso 182999

type M = [(String, Integer)]

leer :: M -> String -> Integer
leer [] x = error "No se encuentra el valor."
leer (m:ms) x
    | ((fst m) == x) = snd m 
    | otherwise = leer ms x
-- leer [("x", 3), ("y", 5)] "x"
-- leer [("x", 3), ("y", 5)] "z"

escribir :: M -> String -> Integer -> M
escribir [] c v = [(c, v)]
escribir (m:ms) c v
    | ((fst m) == c) = (c, v) : ms
    | otherwise = m : (escribir ms c v)

-- escribir [] "x" 3
-- escribir [("x", 5)] "x" 9
-- escribir [("x", 5)] "y" 7
-- escribir [("x", 5), ("y", 3)] "y" 7
-- escribir [("x", 5), ("y", 3), ("z", 4)] "y" 7

data E = L Integer
    | V String
    | String := E
    | E :- E 
    | E :+ E
    | E :* E
    | E :/ E
    | E :% E

eval :: M -> E -> (M, Integer)
eval m (L z) = (m, z)
-- eval [] (L 4)

eval m (V z) = (m, leer m z)
--  eval [("x", 5), ("y", 3), ("z", 4)] (V "y")
--  eval [("x", 5), ("z", 4)] (V "y")
--  eval [] (V "y")

eval m (v := e) = (escribir (fst (eval m e)) v (snd (eval m e)), snd (eval m e))
-- eval [] ("x" := L 4)
-- eval [("x", 5), ("y", 3), ("z", 4)] ("x" := L 4)

eval m (e1 :- e2) = (fst (eval (fst (eval m e1)) e2), (snd (eval m e1)) - (snd (eval (fst (eval m e1)) e2)))
    
-- Analisis

-- M' = fst (eval m e1)
-- m = snd (eval m e1)

-- M'' = fst (eval (fst (eval m e1)) e2)
-- n = snd (eval (fst (eval m e1)) e2)
--------------------------------------
-- (fst (eval (fst (eval m e1)) e2), (snd (eval m e1)) - (snd (eval (fst (eval m e1)) e2)))

-- Ejemplos
-- eval [] (L 4 :- L 3)
-- eval [("x", 5), ("y", 3), ("z", 4)] (L 4 :- V "z")
-- eval [("x", 5), ("y", 3), ("z", 4)] (L 4 :- ("z" := L 3)) -- En este caso la asignación toma como resultado lo asignado

eval m (e1 :+ e2) = (fst (eval (fst (eval m e1)) e2), (snd (eval m e1)) + (snd (eval (fst (eval m e1)) e2)))
eval m (e1 :* e2) = (fst (eval (fst (eval m e1)) e2), (snd (eval m e1)) * (snd (eval (fst (eval m e1)) e2)))
eval m (e1 :/ e2) = (fst (eval (fst (eval m e1)) e2), div (snd (eval m e1)) (snd (eval (fst (eval m e1)) e2)))
eval m (e1 :% e2) = (fst (eval (fst (eval m e1)) e2), mod (snd (eval m e1)) (snd (eval (fst (eval m e1)) e2)))

-- eval [] (L 4 :+ L 3)
-- eval [("x", 5), ("y", 3), ("z", 4)] (L 4 :+ V "z")

-- eval [] (L 4 :* L 3)
-- eval [("x", 5), ("y", 3), ("z", 4)] (L 4 :* V "z")

-- eval [] (L 4 :/ L 3)
-- eval [("x", 5), ("y", 3), ("z", 0)] (L 4 :/ V "z") excepcion del preludio


-- Casos ejemplo de la letra
-- eval [] (L 13)------------------------------------------------> resultado [], 13
-- eval [] (V "x")-----------------------------------------------> resultado No se encuentra el valor.
-- eval [] (("x" := L 100) :+ (V "x" :* V "x"))------------------> resultado ([("x",100)],10100)
-- eval [] ("x" := ("y" := L 1))---------------------------------> resultado ([("y",1),("x",1)],1)
-- eval [] (("x" := L 0) :+ (("y" := L 1) :/ (V "x"))) ----------> resultado ([("x",0),("y",1)],*** Exception: divide by zero
-- eval [("x", 1)] (("x" := L 100) :+ ("y" := (V "x" :+ V "x")))-> resultado ([("x",100),("y",200)],300)


{-
EJERCICIO 1

    1. Analice la cuarta y  ́ultima regla: ¿Qué puede decir acerca del orden de evaluación? ¿Importa?

    En el caso de la última regla tenemos que el orden efectivamente importa, la misma no es conmutativa. 
    Si en la operación tenemos alguna asignación en uno de los lados del evaluador, entonces el resultado no será el mismo si se cambia el orden de las expresiones, ejemploÑ

    Caso 1:

    eval [("x", 1)] (("x" := L 100) :+ (V "x" :+ V "x"))

    100 + (100 + 100) = 300

    Caso 2:

    eval [("x", 1)] ((V "x" :+ V "x") :+ ("x" := L 100))

    (1 + 1) + 100 = 202

}