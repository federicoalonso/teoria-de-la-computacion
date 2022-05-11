module Tarea2 where

    -- Federico Alonso 182999

    data E = Ce String 
            | Cee String [E]
    
    data V = Cv String [V]

    data I = String := E
            | Case String [B]
            | While String [B]

    type M = [(String, V)]

    type P = [I]

    type B = (String,([String],P))

    leer :: M -> String -> V
    leer m x = case (lookup x m) of {
        Nothing -> error "Error: Faltan argumentos en la memoria.";
        Just v -> v;
    }
    
    escribir :: M -> String -> V -> M
    escribir [] c v = [(c, v)]
    escribir (m:ms) c v
        | ((fst m) == c) = (c, v) : ms
        | otherwise = m : (escribir ms c v)

    buscarEnRama :: String -> [B] -> ([String], P)
    buscarEnRama x bs = case (lookup x bs) of {
        Nothing -> error "Error: Faltan argumentos en las ramas.";
        Just e -> e;
    }

    -- Hay que hacer un evaluarTodas??
    evaluarTodas :: M -> [E] -> [V]
    evaluarTodas m [] = []
    evaluarTodas m (e:es) = (evaluacion m e) : (evaluarTodas m es)

    evaluacion :: M -> E -> V
    evaluacion m (Ce x) = leer m x
    -- evaluacion m (Cee c es) = case (evaluacion m (head es)) of {
    --     Cv y vs -> Cv y (map (\e -> evaluacion m e) (tail es));
    --     _ -> error "Error: Faltan argumentos en la memoria.";
    -- }
    evaluacion m (Cee c es) = case evaluarTodas m es of {
        vs -> Cv c vs;
        _ -> error "Error: Faltan argumentos en la memoria.";
    }