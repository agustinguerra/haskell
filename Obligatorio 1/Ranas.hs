-- Agustin Guerra
-- 196344
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Ranas where

data Rana where {V :: Rana ; M :: Rana}  deriving (Eq, Show)

type Estado = ([Rana],[Rana])

data Mov where {AM :: Mov ; SM :: Mov ; AV :: Mov ; SV :: Mov } deriving (Eq, Show)

imprimirRana :: Rana -> String
imprimirRana = \r -> case r of {
 V->"V";
 M->"M"
}

inicial :: Estado
inicial = ([V,V,V],[M,M,M])

final :: Estado
final = ([M,M,M],[V,V,V])

mostrarSegundo :: [Rana] -> String
mostrarSegundo = \r -> case r of {
 []->"";
 x:xs->(imprimirRana x) ++ mostrarSegundo xs
}

mostrar :: Estado -> String
mostrar = \e -> case e of {
 (x,y)->case x of {
  []->"_" ++ mostrarSegundo y ;
  x:xs->(imprimirRana x) ++ mostrar (xs,y)
  }
}

validarAM :: Estado -> Bool
validarAM = \(_,y) -> case y of {
 []->False;
 y:_->y == M
}

validarAV :: Estado -> Bool
validarAV = \(x,y) -> case x of {
 []->False;
 x->last x == V
}

validarSM :: Estado -> Bool
validarSM = \(x,y) -> case y of {
 []->False;
 y:ys->length (y:ys) > 1 && y == V && head ys == M
}

validarSV :: Estado -> Bool
validarSV = \(x,y) -> case x of{
 []->False;
 x->length x > 1 && last x == M && head (tail (reverse x))==V
}

valido :: Estado -> Mov -> Bool
valido = \(x,y) -> \m -> case m of {
 AM->validarAM (x,y);
 AV->validarAV (x,y);
 SV->validarSV (x,y);
 SM->validarSM (x,y)
}

moverAM :: Estado -> Estado
moverAM = \(x,y) -> case valido (x,y) AM of {
 True -> (x ++ [head y], tail y);
 False -> (x,y)
}

moverSM :: Estado -> Estado
moverSM = \(x,y) -> case valido (x,y) SM of {
 True -> (x++[head(tail y)]++[head y],tail(tail y));
 False -> (x,y)
}

moverAV :: Estado -> Estado
moverAV = \(x,y) -> case valido (x,y) AV of {
 True -> (init x, [last x] ++ y);
 False -> (x,y)
}

moverSV :: Estado -> Estado
moverSV = \(x,y) -> case valido (x,y) SV of {
 True -> (init (init x) , [last x] ++ [last (init x)] ++ y);
 False -> (x,y)
}

mover :: Mov -> Estado ->  Estado
mover = \m -> \(x,y) -> case m of {
 AM -> moverAM (x,y);
 SM -> moverSM (x,y);
 AV -> moverAV (x,y);
 SV -> moverSV (x,y);
}

movimientosDado :: Mov -> Estado -> [(Mov, Estado)]
movimientosDado = \m -> \(x,y) -> case valido (x,y) m of {
 True -> (m,mover m (x,y)):[];
 False ->  []
}

movimientos :: Estado -> [(Mov, Estado)]
movimientos =  \(x,y) -> movimientosDado AM (x,y) ++ movimientosDado SM (x,y) ++ movimientosDado AV (x,y) ++ movimientosDado SV (x,y)

gano :: Estado -> Bool
gano =  \(x,y) ->  (x,y) ==  ([M,M,M],[V,V,V])

perdio :: Estado -> Bool
perdio = \(x,y) -> case (valido (x,y) AM || valido (x,y) AV || valido (x,y) SM || valido (x,y) SV) || gano (x,y) of {
 True -> False;
 False -> True
}
