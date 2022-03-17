{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Polinomios where
-- 182999 - Federico Alonso
-- 208443 - Cristian Palma

data ExprPol where {
		Pol :: Polinomio -> ExprPol ;
		Der :: Polinomio -> ExprPol ;
		Eval :: Polinomio -> Int -> ExprPol ;
		Sum :: ExprPol -> ExprPol -> ExprPol ;
		Prod :: ExprPol -> ExprPol -> ExprPol 
		}

type Monomio = (Int, Int)
type Polinomio = [Monomio]

agregarMon :: Monomio-> Polinomio -> Polinomio
agregarMon = \m -> \p -> case p of {
	[] -> case (fst (m)) == 0 of{
			True -> [];
			False -> m:[]
	};
	x:xs -> case (snd (x)) < (snd m) of {

				True -> m:p;
				False -> case (snd x) == (snd m) of {

								True -> case (fst (x)) + (fst (m)) == 0 of {

									True -> xs;
									False -> ((fst (x)) + (fst (m)), (snd (m))):xs

									};	
								False -> x:(agregarMon m xs)

								}
				}
}
	
monXmon:: Monomio -> Monomio -> Monomio
monXmon= \m1 -> \m2 -> ((fst m1) * (fst m2), (snd m1) + (snd m2))

monXPol:: Monomio -> Polinomio -> Polinomio
monXPol= \m -> \p -> case p  of {
		[] -> [];
		x:[] -> (monXmon x m) : [];
		x:y:xs -> (monXmon x m):(monXPol m (y:xs))
}

--1)
redPol :: Polinomio -> Polinomio
redPol = \p -> case p of {
	[] -> [];
	x:xs -> agregarMon x (redPol xs)
}

--2)
sumPol :: Polinomio -> Polinomio -> Polinomio
sumPol = \p1 -> \p2 -> case p1 of {
	[] -> redPol p2;
	x:xs -> redPol (x:(sumPol xs p2))
}

--3)
mulPol :: Polinomio -> Polinomio -> Polinomio
mulPol = \p1 -> \p2 -> case p1 of {
	[] -> [];
	x:[] -> redPol (monXPol x p2);
	x:y:xs -> redPol ((monXPol x p2) ++ (mulPol (y:xs) p2))
}

--4)
derPol :: Polinomio -> Polinomio
derPol = \p -> case p of {
		[] -> [];
		x:xs ->  redPol(((fst x) * (snd x), (snd x) - 1):(derPol xs))	
}	

--5)
evalPol :: Polinomio -> Int -> Int
evalPol = \p -> \i -> case p of {
	[] -> error "Polinomio Nulo";
	x:[] -> (fst x) * (i ^ (snd x));
	x:xs -> ((fst x) * (i ^ (snd x))) + (evalPol xs i)
}

--6)
gradoPol::Polinomio -> Int
gradoPol = \p -> case p of {
	[] -> error "Polinomio Nulo";
	x:[] -> snd x;
	x:xs -> case (snd x) >= (gradoPol xs) of {
		True -> snd x;
		False -> gradoPol xs
	}
}

--7)
showMon :: Monomio -> String
showMon = \m -> case (snd m) of {
		
			0 -> show (fst m);

			1 ->  case (fst m) of {
				
					0 -> show 0;
					1 -> "x";
					-1 -> "-x";
					y -> show (fst m) ++ "x"
			};

			x ->  case (fst m) of {

					0 -> show 0;
					1 -> "x^" ++ show (snd m);
					-1 -> "-x^" ++ show (snd m);
					y -> show (fst m) ++ "x^" ++ show (snd m)
		}	
}

--8)
showPol :: Polinomio -> String
showPol = \p -> case  p of {
	[] -> "";
	x:[] -> showMon x;
	x:y:xs -> case (fst y) > 0 of {
		
			True -> showMon x ++ "+" ++ showPol (y:xs);
			False -> showMon x ++ showPol (y:xs)
		}
	
}

--9) 
gradoEP :: ExprPol -> Int
gradoEP = \ep -> case ep of {
	Pol x -> gradoPol x;
	Der x -> gradoPol x;
	Eval x v -> gradoPol x;
	Sum ep1 ep2 -> case (gradoEP ep1) >= (gradoEP ep2) of {
		True -> gradoEP ep1;
		False -> gradoEP ep2
	};
	Prod ep1 ep2 -> case (gradoEP ep1) >= (gradoEP ep2) of {
		True -> gradoEP ep1;
		False -> gradoEP ep2
	};
}
	
--10)
calcEP :: ExprPol -> Polinomio
calcEP = \ep -> case ep of {
	Pol x ->  redPol x;
	Der x -> derPol x;
	Eval x v -> [(evalPol x v, 0)];
	Sum ep1 ep2 -> sumPol (calcEP ep1) ( calcEP ep2);
	Prod ep1 ep2 -> mulPol (calcEP ep1) ( calcEP ep2)
}

--11) 
resultado :: ExprPol -> String
resultado = \ep -> showPol (calcEP ep);