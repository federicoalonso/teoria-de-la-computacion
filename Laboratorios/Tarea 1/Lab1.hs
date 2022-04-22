module Lab2 where

    -- Federico Alonso 182999

    type B = (String,([String],E))

    data E = X String
        | C String
        | Lam [String] E
        | App E [E]
        | Case E [B]
        | Rec String E deriving (Show)
        -- | Rec [(String,E)] deriving (Show)

        -- Cuando evaluemos ponemos e =>(abajo) e , pero debe retornar un valor
        -- tiene que ser C [v] | lam x.e
        -- Clase 20-4 explicacion del obligatorio

    type Sigma = [(String,E)]

    buscar :: String -> Sigma -> E
    buscar x [] = error "No se encuentra el valor."
    buscar x (m:ms)
        | ((fst m) == x) = snd m 
        | otherwise = buscar x ms
    -- buscar "x" [("x",C "0")]

    baja :: [String] -> Sigma -> Sigma
    baja xs [] = []
    baja xs (m:ms)
        | (elem (fst m) xs) = baja xs ms
        | otherwise = m:(baja xs ms)
    -- baja ["x"] [("x",C "0")] = []
    -- baja ["x"] [("x",C "0"), ("y", C "1")] = [("y",C "1")]
    -- baja ["x", "y"] [("x",C "0"), ("y", C "1")] = []
    -- baja ["x", "y"] [("z",C "0"), ("y", C "1")] = [("z",C "0")]

    sust :: E -> Sigma -> E
    sust (X x) s = case (buscar x s) of {
        error -> X x;
        otherwise -> buscar x s;
    }

    -- sust (X "x") [("x", C "0")] = C "0"
    -- sust (X "x") [("y", C "1")] = No se encuentra el valor. | X "x"

    sust (C x) s = C x

    -- sust (C "0") [("x", C "5")] = C "0"

    sust (Lam xs e) s = Lam xs (sust e (baja xs s))

    -- sust (Lam ["x"] (X "x")) [("x", C "0")] = No se encuentra el valor.
    -- sust (Lam ["x"] (X "x")) [("y", C "0")] = No se encuentra el valor.
    -- sust (Lam ["x"] (X "y")) [("y", C "0"), ("x", C "1")] = Lam ["x"] (C "0")
    -- sust (Lam ["x"] (X "x")) [("y", C "0"), ("x", C "1")] = No se encuentra el valor | Lam ["x" (X "x")]
    -- primero hace baja
    -- sust (Lam ["x"] (X "x")) [("y", C "0"), ("x", C "1")] = Lam ["x"] (sust (X "x") (baja ["x"] [("y", C "0"), ("x", C "1")]))
    -- sust (Lam ["x"] (X "x")) [("y", C "0"), ("x", C "1")] = Lam ["x"] (sust (X "x") [("y",C "0")])
    