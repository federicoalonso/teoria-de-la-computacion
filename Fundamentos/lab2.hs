{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}




module Lab2 where

data N where {O::N ; S::N -> N} deriving Show



pos::N -> Bool
pos = \n-> case n of {O -> False ; S x -> True}

uno::N
uno = S O

dos::N
dos = S uno

tres::N
tres = S dos

predl::N->N
predl = \n-> case n of {O -> O ; S x ->  x}

par::N->Bool
par = \n-> case n of {O -> True ; S x -> not (par x)}

i2n::Int->N
i2n= \x-> case x of {
			0 -> O ; 
			m -> S (i2n (m-1))
		}

n2i::N->Int
n2i= \x-> case x of {
			O -> 0 ; 
			S x -> 1 + (n2i x)
		}

impar::N->Bool
impar= \n-> case n of {
			O -> False ; 
			S x -> not (impar x)
		}		

impar1::N->Bool
impar1= \n-> not (par n)

doble::N->N
doble= \n-> case n of {
			O -> O ; 
			S x -> S (S (doble x))
		}

triple::N->N
triple= \n-> case n of {
			O -> O ; 
			S x -> S(S (S (triple x)))
		}

cuadruple::N->N
cuadruple= \n-> doble (doble n)


existe::N->(N->Bool)->Bool
existe= \n-> \p-> case n of {
			O -> p O ; 
			S x -> (p x) || (existe x p)
		}

todos::N ->(N->Bool)->Bool
todos= \n-> \p-> case n of {
			O -> p O ; 
			S x -> (p x) && (todos x p)
		}

contar::N->(N->Bool)->N	
contar= \n-> \p-> case n of {
			O -> case p O of {
				True -> S O ; 
				False -> O
		}; 
			S x -> case p (S x) of {
				True -> S (contar x p) ; 
				False -> contar x p
		};
		}


instance Eq N where
  (==) = \n1-> \n2-> case n1 of {
			O -> case n2 of {
					O -> True;
					S x -> False
			};
			S x -> case n2 of {
					O -> False;
					S y -> x == y
			};
		}


instance Ord N where{
	
	(<=) = 	\n1-> \n2-> case n1 of {
			O -> True;
			S x -> case n2 of {
					O -> False;
					S y -> x <= y
			};
		}
}
 
