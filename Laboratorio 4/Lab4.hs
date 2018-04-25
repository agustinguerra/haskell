{-# LANGUAGE NPlusKPatterns #-}
module Lab4 where
import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,concat)

pruebaUnoInt::[Int]
pruebaUnoInt = 11:34:[]

pruebaDosInt::[Int]
pruebaDosInt = 23:32:64:[]

pruebaUnoBool::[Bool]
pruebaUnoBool = True:True:True:[]

pruebaDosBool::[Bool]
pruebaDosBool = True:False:True:[]

null::[a]-> Bool
null = \l-> case l of {
 []->True;
 _:_->False
}

length::[a]-> Int
length = \l-> case l of {
 []->0;
 _:xs->1+(length xs)
}

duplicate::[a] -> [a]
duplicate = \l-> case l of {
 []->[];
 x:xs->x:x:(duplicate xs)
}

sum::[Int]-> Int
sum = \l-> case l of {
 []->0;
 x:xs->x+sum xs
}

prod::[Int]-> Int
prod = \l-> case l of {
 []->1;
 x:xs->x*prod xs
}

map::(a->b) ->[a]-> [b]
map = \f -> \l -> case l of {
 []->[];
 x:xs->(f x):map f xs
}

zip::[a]-> [b]-> [(a,b)]
zip = \a -> \b -> case a of {
 []->[];
 x:xs-> case b of {
  []->[];
  y:ys->(x,y):zip xs ys
  }
}

zipWith::(a->b->c)-> [a]-> [b]-> [c]
zipWith = \f -> \a -> \b -> case a of {
 []->[];
 x:xs-> case b of {
  []->[];
  y:ys->(f x y):zipWith f xs ys
  }
}

filter::(a->Bool) ->[a]-> [a]
filter = \f -> \a -> case a of {
 []->[];
 x:xs-> case f x of {
  True->x:filter f xs;
  False->filter f xs
  }
}

and::[Bool]-> Bool
and = \l -> case l of {
 []->True;
 x:xs->case x of {
  True->and xs;
  False->False
  }
}

or::[Bool]-> Bool
or = \l -> case l of {
 []->False;
 x:xs->case x of{
  True->True;
  False-> or xs
  }
}

cuantos::(a->Bool) ->[a]-> Int
cuantos = \f -> \l -> case l of {
 []->0;
 x:xs-> case f x of {
  True->1+cuantos f xs;
  False->cuantos f xs
  }
}

any::(a->Bool) ->[a]->Bool
any = \f -> \l -> case l of {
  []->False;
  x:xs-> case f x of {
  True->True;
  False->any f xs
  }
}

all::(a->Bool) ->[a]->Bool
all = \f -> \l -> case l of {
 []->True;
 x:xs->case f x of {
  True->all f xs;
  False->False
  }
}

(++)::[a]-> [a]-> [a]
(++) = \a -> \b -> case a of {
 []->b;
 x:xs->x:(xs++b)
}

reverse::[a]-> [a]
reverse = \a -> case a of {
 []->[];
 x:xs->reverse xs++(x:[])
}

concat::[[a]]-> [a]
concat = \a -> case a of {
 []->[];
 x:xs->case x of {
  []->concat xs;
  y:ys->x++concat xs
  }
}

lensum::[[a]] -> Int
lensum = \a -> case a of {
 []->0;
 x:xs->case x of {
  []->lensum xs;
  y:ys->length x+lensum xs
  }
}
