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

    -- buscar :: String -> Sigma -> E
    -- buscar x [] = error "No se encuentra el valor."
    -- buscar x (m:ms)
    --     | ((fst m) == x) = snd m 
    --     | otherwise = buscar x ms
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

    sustbranches :: [B] -> Sigma -> [B]
    sustbranches [] sigma = []
    sustbranches ((x, (xs, e)) : bs) sigma = (x,(xs,sust e (baja xs sigma))):(sustbranches bs sigma)
    -- sustbranches [("x",(["x"],C "0"))] [("x",C "0")] = [("x",(["x"],C "0"))]
    -- sustbranches [("True", ([],C "0")), ("False", ([], C "1"))] [("z",C "0"), ("y", C "1")] = [("True",([],C "0")),("False",([],C "1"))]
    -- sustbranches [("True", (["z"],X "z")), ("False", ([], C "1"))] [("z",C "0"), ("y", C "1")] = [("True",(["z"],X "z")),("False",([],C "1"))]
    -- sustbranches [("True", (["y"],X "z")), ("False", ([], C "1"))] [("z",C "0"), ("y", C "1")] = [("True",(["y"],C "0")),("False",([],C "1"))]

    -- #############################################################################
    -- ################################ SUSTITUCION ################################
    -- #############################################################################

    sust :: E -> Sigma -> E
    sust (X x) s = case (lookup x s) of {
        Nothing -> X x;
        Just e -> e;
    }

    -- sust (X "x") [("x", C "0")] = C "0"
    -- sust (X "x") [("y", C "1")] = X "x"

    sust (C x) s = C x

    -- sust (C "0") [("x", C "5")] = C "0"

    sust (Lam xs e) s = Lam xs (sust e (baja xs s))

    -- sust (Lam ["x"] (X "x")) [("x", C "0")] = Lam ["x"] (X "x")
    -- sust (Lam ["x"] (X "x")) [("y", C "0")] = Lam ["x"] (X "x")
    -- sust (Lam ["x"] (X "y")) [("y", C "0"), ("x", C "1")] = Lam ["x"] (C "0")
    -- sust (Lam ["x"] (X "x")) [("y", C "0"), ("x", C "1")] = Lam ["x" (X "x")]
    
    sust (Case e bs) s = Case (sust e s) (sustbranches bs s)
    -- sust (Case (X "x") [("True", ([], C "0")), ("False", ([], C "1"))]) [("x", C "0")] = Case (C "0") [("True",([],C "0")),("False",([],C "1"))]
    -- sust (Case (X "x") [("x", ([], C "0")), ("False", ([], C "1"))]) [("x", C "0")] = Case (C "0") [("x",([],C "0")),("False",([],C "1"))]

    sust (App e es) s = App (sust e s) (map (\expresion -> sust expresion s) es)
    -- sust (App (X "x") [C "0"]) [("x", C "0")] = App (C "0") [C "0"]
    -- sust (App (X "x") [C "0", X "x", X "y"]) [("x", C "0")] = App (C "0") [C "0",C "0",X "y"]

    sust (Rec x e) s = Rec x (sust e (baja [x] s))
    -- sust (Rec "x" (X "x")) [("x", C "0")] = Rec "x" (X "x")
    -- sust (Rec "y" (X "x")) [("x", C "0")] = Rec "y" (C "0")

    -- ##############################################################################
    -- ################################# EVALUACION #################################
    -- ##############################################################################

    evaltodas :: [E] -> [E]
    evaltodas [] = []
    evaltodas (e:es) = (eval e) : (evaltodas es)

    buscarEnRama :: String -> [B] -> ([String], E)
    buscarEnRama x bs = case (lookup x bs) of {
        Nothing -> error "Error: Faltan argumentos en las ramas.";
        Just e -> e;
    }

    eval :: E -> E
    eval (X x) = App (X x) []
    eval (C x) = App (C x) []
    eval (Lam xs e) = Lam xs e
    eval (Case e bs) = case (eval e) of {
        App (C c) vs -> case (buscarEnRama c bs) of {
            (xs, e2) -> case (length xs == length vs) of {
                False -> error "Error: Cantidad de argumentos incorrecta. - Func eval Case e bs.";
                True -> eval (sust e2 (zip xs vs))
            };
            _ -> error "Error: No se puede aplicar con otra expresion. - Func eval Case e bs. - Segundo case."
        };
        _ -> error "Error: No se puede aplicar con otra expresion. - Func eval Case e bs. - Primer case."
    }
    eval (App e es) = case (eval e) of {
        App (C c) v1s -> case (evaltodas es) of {
            v2s -> App (C c) (v1s ++ v2s);
        };
        Lam xs e2 -> case (evaltodas es) of {
            v1s -> case (length xs == length v1s) of {
                False -> error "Error: Cantidad de argumentos incorrecta. - Func eval App e es.";
                True -> eval (sust e2 (zip xs v1s))
            }
        };
        _ -> error "Error: No se puede aplicar con otra expresion. - Func eval App e es. - Primer case."
    }
    eval (Rec x e) = eval (sust e (zip [x] [eval (Rec x e)]))

    -- #############################################################################
    -- ################################# FUNCIONES #################################
    -- #############################################################################

    elAnd :: E
    elAnd = Lam ["b1", "b2"] (Case (X "b1") [("True",([],X "b2")),("False",([],C "False"))])
    -- eval (App elAnd [(C "True"),(C "False")]) = App (C "False") []
    -- eval (App elAnd [(C "True"),(C "True")]) = App (C "True") []
    -- eval (App elAnd [(C "False"),(C "False")]) = App (C "False") []
    -- eval (App elAnd [(C "True"),(C "True")]) = App (C "True") []

    dupChi :: E
    dupChi = Rec "dup" (Lam ["n"] (Case (X "n") [("0",([],C "0")),("S",(["x"],App (C "S") [App (C "S") [App (X "dup") [X "x"]]]))]))
    -- eval (App dupChi [(C "0")]) = App (C "0") []
    -- eval (App dupChi [App (C "S") [App (C "0") []]]) = App (C "S") [App (C "S") [App (C "0") []]]
    -- eval (App dupChi [App (C "S") [App (C "S") [App (C "0") []]]]) = App (C "S") [App (C "S") [App (C "S") [App (C "S") [App (C "0") []]]]]

    unirChi :: E
    unirChi = Rec "unir" (Lam ["l1", "l2"] (Case (X "l1") [
            ("[]",([],X "l2")),
            (":",(["x", "xs"],App (C ":") [X "x", App (X "unir") [X "xs", X "l2"]]))
        ]))
    -- eval (App unirChi [(C "[]"),(C "[]")]) = App (C "[]") []
    -- eval (App unirChi [(App (C ":") [C "1", C "[]"]),(C "[]")]) = App (C ":") [App (C "1") [],App (C "[]") []]

    -- La funcion reducir no es chi, pero nos ayuda a ver mejor las listas
    reducir :: E -> E
    reducir (App e es) = case es of {
        [] -> e;
        ex -> App e (map reducir ex);
    }
    reducir (X x) = X x
    reducir (C c) = C c
    reducir (Lam xs e) = Lam xs (reducir e)
    reducir (Case e bs) = Case (reducir e) (map (\(x,(xs,e)) -> (x,(xs,reducir e))) bs)
    reducir (Rec x e) = Rec x (reducir e)

    unir :: E -> E
    unir e = reducir(e)
    -- unir (eval (App unirChi [(C "[]"),(C "[]")])) = C "[]"
    -- unir (eval (App unirChi [(App (C ":") [C "1", C "[]"]),(C "[]")])) = App (C ":") [C "1",C "[]"]
    -- unir (eval (App unirChi [(App (C ":") [C "1", C "[]"]),(App (C ":") [C "1", C "[]"])])) = App (C ":") [C "1",App (C ":") [C "1",C "[]"]]
    
    cero :: E
    cero = C "0"

    uno :: E
    uno = App (C "S") [cero]

    dos :: E
    dos = App (C "S") [uno]

    tres :: E
    tres = App (C "S") [dos]

    lista01 :: E
    lista01 = App (C ":") [cero, App (C ":") [uno, C "[]"]]

    lista23 :: E
    lista23 = App (C ":") [dos, App (C ":") [tres, C "[]"]]
    -- unir (eval (App unirChi [lista01, lista23])) = App (C ":") [C "0",App (C ":") [App (C "S") [C "0"],App (C ":") [App (C "S") [App (C "S") [C "0"]],App (C ":") [App (C "S") [App (C "S") [App (C "S") [C "0"]]],C "[]"]]]]

    ramaIChi :: E
    ramaIChi = Rec "ramaI" (
            Lam ["a"] (
                Case (X "a") [
                    ("H", ([], C "[]")),
                    ("N", (["v", "i", "d"], (App (C ":") [X "v", (App (X "ramaI") [X "i"])])))
                ]
            )
        )
    -- eval (App ramaIChi [arbol2]) = App (C ":") [App (C "0") [],App (C ":") [App (C "0") [],App (C "[]") []]]
    
    -- Igual que la funcion reducir, esta nos ayuda a ver mejor las listas
    ramaI :: E -> E
    ramaI e = reducir(e)
    
    hoja :: E
    hoja = App (C "H") []
    -- ramaI (eval (App ramaIChi [hoja])) = C "[]"

    arbol1 :: E
    arbol1 = App (C "N") [cero, hoja, hoja]
    -- ramaI (eval (App ramaIChi [arbol1])) = App (C ":") [C "0",C "[]"]
 
    arbol2 :: E
    arbol2 = App (C "N") [cero, arbol1, arbol1]
    -- ramaI (eval (App ramaIChi [arbol2])) = App (C ":") [C "0",App (C ":") [C "0",C "[]"]]

    arbol3 :: E
    arbol3 = App (C "N") [cero, arbol2, hoja]
    -- ramaI (eval (App ramaIChi [arbol3])) = App (C ":") [C "0",App (C ":") [C "0",App (C ":") [C "0",C "[]"]]]

    arbol31 :: E
    arbol31 = App (C "N") [uno, arbol2, arbol1]
    -- ramaI (eval (App ramaIChi [arbol31])) = App (C ":") [App (C "S") [C "0"],App (C ":") [C "0",App (C ":") [C "0",C "[]"]]]

    arbol33 :: E
    arbol33 = App (C "N") [tres, arbol1, arbol2]
    -- ramaI (eval (App ramaIChi [arbol33])) = App (C ":") [App (C "S") [App (C "S") [App (C "S") [C "0"]]],App (C ":") [C "0",C "[]"]]

    -- #############################################################################
    -- ############################### CASOS FINALES ###############################
    -- #############################################################################

    eTrue :: E
    eTrue = C "True"

    eFalse :: E
    eFalse = C "False"

    -- eval (App elAnd [eTrue,eTrue,eFalse]) = Error: Cantidad de argumentos incorrecta. - Func eval App e es.

    notMal :: E
    notMal = Lam ["x"] (Case (X "y") [
            ("True",([], C "False")),
            ("False",([], C "True"))
        ])
    -- eval (App notMal [eTrue]) = Error: No se puede aplicar con otra expresion. - Func eval Case e bs. - Primer case.

    lista13 :: E
    lista13 = App (C ":") [uno, App (C ":") [tres, C "[]"]]

    lista130 :: E
    lista130 = App (C ":") [lista13, App (C ":") [cero, C "[]"]]
    -- unir (eval (App unirChi [(lista130), eFalse])) = App (C ":") [App (C ":") [App (C "S") [C "0"],App (C ":") [App (C "S") [App (C "S") [App (C "S") [C "0"]]],C "[]"]],App (C ":") [C "0",C "False"]]