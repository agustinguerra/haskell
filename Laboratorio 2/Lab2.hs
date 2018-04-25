{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module Lab2 where
import Prelude (Show)
import Lab1

data N where {O::N ;  S::N -> N}
 deriving Show

pos::N -> Bool
pos = \n-> case n  of {O -> False;
S x -> True}

uno::N
uno = S O
dos::N
dos = S uno
tres::N
tres = S dos
cuatro::N
cuatro = S tres

pred::N->N
pred = \n-> case n of {O -> O;
S k -> k}

par::N->Bool
par = \n-> case n of {O -> True;
S k -> not (par k)}

impar::N->Bool
impar = \n-> case n of {O -> False;
S k -> not (impar k)}

imp::N->Bool
imp = \n-> not (par n)

doble::N->N
doble = \n -> case n of {O -> O;
S k -> S (S(doble k))}

triple::N->N
triple = \n -> case n of {O -> O;
S k -> S(S(S(triple k)))}

contar::N->(N->Bool)->N
contar = \n -> \p -> case n of {
	O -> case p O of {
		False -> O;
		True -> S O
	};
	S k -> case p (n) of {
		False -> contar k p;
		True -> S(contar k p)
	}
} 

existe::N->(N->Bool)->Bool
existe = \n -> \p -> case n of {
	O -> p O;
	S k -> case p (S k) of{
		False -> existe k p;
		True -> True
	}
}

todos::N ->(N->Bool)->Bool
todos = \n -> \p -> case n of {
	O -> p O;
	S k -> case p(S k) of {
		False -> False;
		True -> todos k p
	}
}

(+)::N->N->N
(+) = \n -> \t -> case t of {
	O -> n;
	S k -> S (n + k)
} 

(*)::N->N->N
(*)= \n -> \t -> case t of {
	O -> O;
	S k -> n + (k*n)
}

(^ )::N->N->N
(^ )= \n -> \t -> case t of {
	O -> uno;
	S k -> n*(n^ k)
}

fact::N->N
fact= \n -> case n of {
	O -> uno;
	S k -> (S k)*(fact k)
}

sumi::N->N
sumi= \n -> case n of {
	O -> O;
	S k -> (S k)+(sumi k)
}

sumdobles::N->N
sumdobles= \n -> case n of {
	O -> O;
	S k -> doble(S k)+(sumdobles k)
}

sumfi::(N->N)->N->N
sumfi= \p -> \n -> case n of {
	O->p O;
	S k -> p(S k)+(sumfi p k)
}

sumpares::N->N
sumpares= \n -> case n of {
	O -> O;
	S k -> case par(S k) of {
		True -> (S k)+sumpares(k);
		False -> sumpares(k)
	}
}

sumpi::(N->Bool)->N->N
sumpi= \p -> \n -> case n of {
	O -> O;
	S k -> case p(S k) of {
		True -> (S k)+(sumpi p k);
		False -> sumpi p k
	}
}

class Eq a where
	(==),(/=)::a->a->Bool
	(/=) = \x y -> not (x==y)

class Eq a => Ord a  where
	(<),(<=),(>=),(>)::a->a->Bool
	(<=) = \x y -> (x < y) || (x == y)
	(>=) = \x y -> not (x < y)
	(>) =  \x y -> not (x <= y)

instance Eq Bool where{
	(==) = \n -> \t -> case n of {
		False-> not t;
		True->t
	}
}

instance Eq N where
	(==) = \n -> \t -> case n of {
		O -> case t of {
				O -> True;
				S k -> False;
			};
		S k -> case t of {
				O -> False;
				S x -> k == x 
		}	
	}

instance Ord N where
	(<) = \n -> \t -> case n of {
		O -> case t of {
				O -> False;
				S k -> True
			};
		S k -> case t of {
				O -> False;
				S x -> k < x 
		}	
	}

minimo::N -> N -> N
minimo= \n -> \t -> case (n<t) of {
	True -> n;
	False -> t
}

maximo::N -> N -> N
maximo= \n -> \t -> case (n<t) of {
	True -> t;
	False -> n
}

min3::N -> N -> N -> N
min3= \n -> \t -> \q -> case (minimo n t)<q of {
	True -> (minimo n t);
	False -> q
}