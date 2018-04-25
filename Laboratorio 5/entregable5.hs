{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}

type Var = String
type Mem = [(Var,Int)]

final :: Mem
final = [("A",5),("B",2),("C",6)]

{- Filter recibe como parametro una exp lambda, que permite
  filtrar las tuplas donde el primer valor sea igual que el de v -}

(@@) :: Var -> Mem -> Int
(@@) = \ v m -> case filter (\(z,_) -> z == v) m of {
    [] -> 0;
    x:_ -> case x of (a,b) -> b;
}

{- Filter recibe como parametro una exp lambda, que me permite filtrar
 la memoria, devolviendo toda la memoria sin incluir la variable
 que se desea actualizar, y la agregar al final de la nueva lista -}

upd :: (Var, Int) -> Mem -> Mem
upd = \v m -> case v of {
    (a,_) -> case filter (\(z,_) -> z /= a) m of {
        [] -> [v];
        x:sx -> x:sx ++ [v]
    }
}

