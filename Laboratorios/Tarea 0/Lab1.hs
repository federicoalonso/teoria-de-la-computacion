module Lab1 where

type M = [(String, Integer)]

leer :: M -> String -> Integer
leer [] x = error "No se encuentra el valor."
leer (m:ms) x
    | ((fst m) == x) = snd m 
    | otherwise = leer ms x
-- leer [("x", 3), ("y", 5)] "x"

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
    -- | E :- E 
    -- | E :+ E
    -- | E :* E
    -- | E :/ E
    -- | E :% E

eval :: M -> E -> (M, Integer)
eval m (L z) = (m, z)
-- eval [] (L 4)

eval m (V z) = (m, leer m z)
--  eval [("x", 5), ("y", 3), ("z", 4)] (V "y")
--  eval [("x", 5), ("z", 4)] (V "y")
--  eval [] (V "y")

-- eval m (V v := e) = (escribir (fst (eval m e)) v (snd (eval m e)), snd (eval m e))


-- El orden de la cuarta regla si importa, ejemplo: no es conmutativa
-- primero e1 nos muestra, puede cambiar la memoria, y luego evalua en la segunda que tambien puede cambiar la memoria
-- usar div y mod, no / y %
-- el dos da error
-- division entre cero?