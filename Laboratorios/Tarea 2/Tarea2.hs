module Tarea2 where

    -- Federico Alonso 182999

    data E = Ce String 
            | Cee String [E]
                deriving Show
    
    data V = Cv String [V]
                deriving Show

    data I = [String] := [E]
            | Case String [B]
            | While String [B]
                deriving (Show)

    type M = [(String, V)]

    type P = [I]

    type B = (String,([String],P))

    leer :: M -> String -> V
    leer m x = case (lookup x m) of {
        Nothing -> error "Error: Faltan argumentos en la memoria. Leer.";
        Just v -> v;
    }
    
    escribir :: M -> String -> V -> M
    escribir [] c v = [(c, v)]
    escribir (m:ms) c v
        | ((fst m) == c) = (c, v) : ms
        | otherwise = m : (escribir ms c v)
    
    escribirTodas :: M -> [(String, V)] -> M
    escribirTodas m [] = m
    escribirTodas m ((c, v):ms) = escribirTodas (escribir m c v) ms

    buscarEnRama :: String -> [B] -> ([String], P)
    buscarEnRama x bs = case (lookup x bs) of {
        Nothing -> error "Error: Faltan argumentos en las ramas.";
        Just e -> e;
    }

    evaluarTodas :: M -> [E] -> [V]
    evaluarTodas m [] = []
    evaluarTodas m (e:es) = (evaluacion m e) : (evaluarTodas m es)

    evaluacion :: M -> E -> V
    evaluacion m (Ce x) = leer m x
    evaluacion m (Cee x es) = Cv x (evaluarTodas m es)


    ejecucionCompleta :: M -> P -> M
    ejecucionCompleta m [] = m
    ejecucionCompleta m (i:ps) = case (ejecucionPrimInst m (i:ps)) of {
        (ms, pss) -> ejecucionCompleta ms pss;
    }

    ejecucionPrimInst :: M -> P -> (M, P)
    ejecucionPrimInst m ((xs := es):p) = case (evaluarTodas m es) of {
        vs -> case (escribirTodas m (zip xs vs)) of {
            ms -> (ms, p);
            _ -> error "Error: Faltan argumentos en la memoria.";
        };
        _ -> error "Error: Las listas de claves y expresiones no tienen el mismo número.";
    }
    ejecucionPrimInst m ((Case x bs) : p) = case (evaluacion m (Ce x)) of {
        Cv y vs -> case (buscarEnRama y bs) of {
            (xs, ps) -> case (escribirTodas m (zip xs vs)) of {
                ms -> (ms, ps ++ p);
                _ -> error "Error: No son la misma cantidad de argumentos.";
            };
            _ -> error "Error: Error bucando en rama de case.";
        };
        _ -> error "Error: Faltan argumentos en la memoria.";
    }
    ejecucionPrimInst m ((While x bs) : p) = case (evaluacion m (Ce x)) of {
        Cv y vs -> case (lookup y bs) of {
            Nothing -> (m, p);
            Just (xs, ps) -> case (escribirTodas m (zip xs vs)) of {
                ms -> (ms, ps ++ [(While x bs)] ++ p);
                _ -> error "Error: No son la misma cantidad de argumentos.";
            };
        };
        _ -> error "Error: Faltan argumentos en la memoria. While";
    }

    cero :: V
    cero = Cv "0" []

    uno :: V
    uno = Cv "S" [cero]

    dos :: V
    dos = Cv "S" [uno]

    tres :: V
    tres = Cv "S" [dos]

    cuatro :: V
    cuatro = Cv "S" [tres]

    cinco :: V
    cinco = Cv "S" [cuatro]

    seis :: V
    seis = Cv "S" [cinco]

    siete :: V
    siete = Cv "S" [seis]

    nott :: P
    nott = [
            (Case "neg" [
                ("True", ([], [["neg"] := [Cee "False" []]])),
                ("False", ([], [["neg"] := [Cee "True" []]]))
            ])
        ]

    true :: V
    true = Cv "True" []

    false :: V
    false = Cv "False" []

    notFalse :: V
    notFalse = evaluacion (ejecucionCompleta [("neg", Cv "False" [])] nott) (Ce "neg")

    notTrue :: V
    notTrue = evaluacion (ejecucionCompleta [("neg", Cv "True" [])] nott) (Ce "neg")

    notNotFalse :: V
    notNotFalse = evaluacion (ejecucionCompleta [("neg", Cv "False" [])] (nott ++ nott)) (Ce "neg")

    notNotTrue :: V
    notNotTrue = evaluacion (ejecucionCompleta [("neg", Cv "True" [])] (nott ++ nott)) (Ce "neg")

    par :: P
    par = [
            (["neg"] := [Cee "True" []]),
            (
                While "esPar" [
                    ("S", (["xs"], (["esPar"] := [Ce "xs"]) : nott))
                ]
            )
        ]
    
    esCeroPar :: V
    esCeroPar = evaluacion (ejecucionCompleta [("esPar", cero)] par) (Ce "neg")

    esUnoPar :: V
    esUnoPar = evaluacion (ejecucionCompleta [("esPar", uno)] par) (Ce "neg")

    esDosPar :: V
    esDosPar = evaluacion (ejecucionCompleta [("esPar", dos)] par) (Ce "neg")

    esTresPar :: V
    esTresPar = evaluacion (ejecucionCompleta [("esPar", tres)] par) (Ce "neg")

    esCuatroPar :: V
    esCuatroPar = evaluacion (ejecucionCompleta [("esPar", cuatro)] par) (Ce "neg")

    suma :: P
    suma = [
            (["pri"] := [Ce "primero"]),
            (["seg"] := [Ce "segundo"]),
            (["resultado"] := [Ce "seg"]),
            While "pri" [
                ("S", (["xs"], [
                    ["pri"] := [Ce "xs"],
                    ["seg"] := [Cee "S" [Ce "seg"]],
                    ["resultado"] := [Ce "seg"]
                ]))
            ]
        ]
    
    sumaCeroCero :: V
    sumaCeroCero = evaluacion (ejecucionCompleta [("primero", cero), ("segundo", cero)] suma) (Ce "resultado")

    sumaCeroUno :: V
    sumaCeroUno = evaluacion (ejecucionCompleta [("primero", cero), ("segundo", uno)] suma) (Ce "resultado")

    sumaDosUno :: V
    sumaDosUno = evaluacion (ejecucionCompleta [("primero", dos), ("segundo", uno)] suma) (Ce "resultado")

    sumaUnoCuatro :: V
    sumaUnoCuatro = evaluacion (ejecucionCompleta [("primero", uno), ("segundo", cuatro)] suma) (Ce "resultado")

    -- Estos no los pedia la letra pero pensé que podía necesitarlos y me quedaron hechos.
    -- Me sirvieron para entender las listas, por eso los dejo.
    listaNoVacia :: P
    listaNoVacia = [
            (Case "lista" [
                ("[]", ([], [["conElementos"] := [Cee "False" []]])),
                (":", ([], [["conElementos"] := [Cee "True" []]]))
            ])
        ]
    
    listaVacia :: V
    listaVacia = Cv "[]" []

    listaUnElemento :: V
    listaUnElemento = Cv ":" [Cv "valor" [], listaVacia]

    listaDosElementos :: V
    listaDosElementos = Cv ":" [Cv "valor" [], listaUnElemento]

    pruebaListaVacia :: V
    pruebaListaVacia = evaluacion (ejecucionCompleta [("lista", listaVacia)] listaNoVacia) (Ce "conElementos")

    pruebaListaUnElemento :: V
    pruebaListaUnElemento = evaluacion (ejecucionCompleta [("lista", listaUnElemento)] listaNoVacia) (Ce "conElementos")
    
    largo :: P
    largo = [
            (["largo"] := [Cee "0" []]),
            (
                While "lista" [
                    (":", (["valor", "xs"], [
                        ["lista"] := [Ce "xs"],
                        ["largo"] := [Cee "S" [Ce "largo"]]
                    ]))
                ]
            )
        ]
    
    listaLargoCero :: V
    listaLargoCero = evaluacion (ejecucionCompleta [("lista", listaVacia)] largo) (Ce "largo")

    listaLargoUno :: V
    listaLargoUno = evaluacion (ejecucionCompleta [("lista", listaUnElemento)] largo) (Ce "largo")

    listaLargoDos :: V
    listaLargoDos = evaluacion (ejecucionCompleta [("lista", listaDosElementos)] largo) (Ce "largo")

    igualdadN :: P
    igualdadN = [
            (["pri"] := [Ce "primero"]),
            (["seg"] := [Ce "segundo"]),
            (["iguales"] := [Cee "False" []]),
            (["terminado"] := [Cee "False" []]),
            While "terminado" [
                ("False", ([], [
                    (Case "pri" [
                        ("0", ([], [
                            (Case "seg" [
                                ("0", ([], [
                                    (["iguales"] := [Cee "True" []])
                                ])),
                                ("S", (["ss"], []))
                            ]),
                            (["terminado"] := [Cee "True" []])
                        ])),
                        ("S", (["pp"], [
                            (Case "seg" [
                                ("0", ([], [
                                    (["terminado"] := [Cee "True" []])
                                ])),
                                ("S", (["ss"], [
                                    (["pri"] := [Ce "pp"]),
                                    (["seg"] := [Ce "ss"])
                                ]))
                            ])
                        ]))
                    ])
                ]))
            ]
        ]
    
    igualesCeroCero :: V
    igualesCeroCero = evaluacion (ejecucionCompleta [("primero", cero), ("segundo", cero)] igualdadN) (Ce "iguales")

    igualesCeroUno :: V
    igualesCeroUno = evaluacion (ejecucionCompleta [("primero", cero), ("segundo", uno)] igualdadN) (Ce "iguales")

    igualesUnoCero :: V
    igualesUnoCero = evaluacion (ejecucionCompleta [("primero", uno), ("segundo", cero)] igualdadN) (Ce "iguales")

    igualesUnoUno :: V
    igualesUnoUno = evaluacion (ejecucionCompleta [("primero", uno), ("segundo", uno)] igualdadN) (Ce "iguales")

    igualesUnoDos :: V
    igualesUnoDos = evaluacion (ejecucionCompleta [("primero", uno), ("segundo", dos)] igualdadN) (Ce "iguales")

    igualesCuatroCuatro :: V
    igualesCuatroCuatro = evaluacion (ejecucionCompleta [("primero", cuatro), ("segundo", cuatro)] igualdadN) (Ce "iguales")

    igualesCuatroTres :: V
    igualesCuatroTres = evaluacion (ejecucionCompleta [("primero", cuatro), ("segundo", tres)] igualdadN) (Ce "iguales")

    fibonacci :: P
    fibonacci = [
            (["primero"] := [Cee "0" []]),
            (["segundo"] := [Cee "S" [Ce "primero"]]),
            (["fibonacci"] := [Cee "0" []]),
            (["resultado"] := [Cee "0" []]),
            While "entrada" [
                ("S", (["xs"], [
                    ["entrada"] := [Ce "xs"],
                    ["primero"] := [Ce "segundo"],
                    ["segundo"] := [Ce "fibonacci"]
                ] ++ suma ++ [
                    ["fibonacci"] := [Ce "resultado"]
                ]))
            ]
        ]
    
    fibonacciCero :: V
    fibonacciCero = evaluacion (ejecucionCompleta [("entrada", cero)] fibonacci) (Ce "fibonacci")

    fibonacciUno :: V
    fibonacciUno = evaluacion (ejecucionCompleta [("entrada", uno)] fibonacci) (Ce "fibonacci")

    fibonacciDos :: V
    fibonacciDos = evaluacion (ejecucionCompleta [("entrada", dos)] fibonacci) (Ce "fibonacci")

    fibonacciTres :: V
    fibonacciTres = evaluacion (ejecucionCompleta [("entrada", tres)] fibonacci) (Ce "fibonacci")

    fibonacciCuatro :: V
    fibonacciCuatro = evaluacion (ejecucionCompleta [("entrada", cuatro)] fibonacci) (Ce "fibonacci")

    fibonacciCinco :: V
    fibonacciCinco = evaluacion (ejecucionCompleta [("entrada", cinco)] fibonacci) (Ce "fibonacci")

    fibonacciSeis :: V
    fibonacciSeis = evaluacion (ejecucionCompleta [("entrada", seis)] fibonacci) (Ce "fibonacci")

    fibonacciSiete :: V
    fibonacciSiete = evaluacion (ejecucionCompleta [("entrada", siete)] fibonacci) (Ce "fibonacci")