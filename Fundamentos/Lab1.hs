{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where

import Prelude (Show)

data Bool where {
                False::Bool ; 
                True::Bool
                }deriving Show

not::Bool->Bool
not = \ b -> case b of {
    False->True; 
    True->False
}

-- Ejercicio 2

(&&)::Bool->Bool->Bool
(&&) = \ b1 -> \ b2 -> case b1 of{
    True -> b2;
    False -> False
}

(||)::Bool->Bool->Bool
(||) = \ b1 -> \ b2 -> case b1 of{
    True -> True;
    False -> b2
}

xor::Bool->Bool->Bool
xor = \ b1 -> \ b2 -> case b1 of{
    True -> not b2;
    False -> b2
}

ni::Bool->Bool->Bool
ni = \ b1 -> \ b2 -> case b1 of{
    True -> False;
    False -> not b2
}

(>>)::Bool->Bool->Bool
(>>) = \ b1 -> \ b2 -> case b1 of{
    True -> b2;
    False -> True
}

-- Ejericio 3

(==)::Bool->Bool->Bool
(==) = \ b1 -> \ b2 -> case b1 of{
    True -> b2;
    False -> not b2
}

(===)::Bool->Bool->Bool
(===) = \ b1 -> \ b2 -> (||) (ni b1 b2) ((&&) b1 b2)

-- OPCION 2
(====)::Bool->Bool->Bool
(====) = \ b1 -> \ b2 -> not (xor b1 b2) 

(/=)::Bool->Bool->Bool
(/=) = \ b1 -> \ b2 -> case b1 of{
    True -> not b2;
    False -> b2
}

(/==)::Bool->Bool->Bool
(/==) = \ b1 -> \ b2 -> not ((===) b1 b2)

(<=)::Bool->Bool->Bool
(<=) = \ b1 -> \ b2 -> (>>) b1 b2


-- Ejercicio 4

(|||)::Bool->Bool->Bool
(|||) = \ b1 -> \ b2 ->  not ((&&) (not b1) (not b2))


(>>>)::Bool->Bool->Bool
(>>>) = \ b1 -> \ b2 -> not ((&&) b1 (not b2))



-- Ejericio 5

unanimidad::Bool->Bool->Bool-> Bool
unanimidad = \ b1 -> \ b2 -> \ b3 -> case b1 of{
    True -> (&&) b2 b3;
    False -> not ((||) b2 b3)
}

-- OPCION 2
unanimidadd::Bool->Bool->Bool->Bool
unanimidadd = \ b1 -> \ b2 -> \ b3 ->case b1 of{
    True -> (&&) b2 b3;
    False -> ni b2 b3
} 

mayoria::Bool->Bool->Bool-> Bool
mayoria = \ b1 -> \ b2 -> \ b3 -> case b1 of{
    True -> (||) b2 b3;
    False -> (&&) b2 b3
}

impar::Bool->Bool->Bool-> Bool
impar = \ b1 -> \ b2 -> \ b3 -> case b1 of{
    True -> (==) b2 b3;
    False -> not ((==) b2 b3)
}

-- OPCION 2
imparr::Bool->Bool->Bool->Bool
imparr = \ b1 -> \ b2 -> \ b3 ->case b1 of{
    True -> (==) b2 b3;
    False -> xor b2 b3
}

-- Ejericio 6

(@@)::Bool->Bool->Bool
(@@) = \ b1 -> \ b2 -> not ((&&) b1 b2)

-- Exactamente uno false da true
(#)::Bool->Bool->Bool
(#) = \ b1 -> \ b2 -> (/=) b1 b2

-- e tri b1 b2 b3 devuelve True si hay mas argumentos False que True.
tri::Bool->Bool->Bool->Bool
tri = \ b1 -> \ b2 -> \ b3 -> case b1 of{
    True -> not ((||) b2 b3);
    False -> not ((&&) b2 b3)
}


--otra forma
trii::Bool->Bool->Bool->Bool
trii = \ b1 -> \ b2 -> \ b3 -> case b1 of{
    True -> ni b2 b3;
    False -> not ((&&) b2 b3)
}