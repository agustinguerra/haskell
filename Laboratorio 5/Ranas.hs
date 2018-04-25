-- Autor(es): Mariano Zunino
-- Numeros de estudiantes: 212687

{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}

module Ranas where

data Rana where {V :: Rana ; M :: Rana}  deriving (Eq, Show)

type Estado = ([Rana],[Rana])

data Mov where {AM :: Mov ; SM :: Mov ; AV :: Mov ; SV :: Mov } deriving (Eq, Show)

inicial :: Estado
inicial = ([V,V,V],[M,M,M])

final :: Estado
final = ([M,M,M],[V,V,V])

{- Retorno el valor de la rana como char -}

char :: Rana -> Char
char = \r -> case r of
    V -> 'V';
    M -> 'M'

mostrar :: Estado -> String
mostrar = \e -> case e of
    (x, y) -> map char x ++ "_" ++ map char y


validarAM :: Estado -> Bool
validarAM = \e -> case e of
    (_,y) -> not (null y) && head y == M

validarAV :: Estado -> Bool
validarAV = \e -> case e of
    (x,_) -> not (null x) && last x == V

validarSM :: Estado -> Bool
validarSM = \e -> case e of
    (_,y) -> length y > 1 && head y == V && head (tail y) == M

validarSV :: Estado -> Bool
validarSV = \e -> case e of
    (x,_) -> length x > 1 && last x == M && last (init x)== V

moverAM :: Estado -> Estado
moverAM = \e -> case  e of
   (x,y) -> (x ++ [head y], tail y)

moverAV :: Estado -> Estado
moverAV = \e -> case e of
   (x,y) -> (init x, [last x] ++ y)

moverSM :: Estado -> Estado
moverSM = \e -> case e of
    (x,y) -> (x++[head(tail y)]++[head y],tail(tail y))

moverSV :: Estado -> Estado
moverSV = \e -> case e of
    (x,y) -> (init (init x) , [last x] ++ [last (init x)] ++ y)


valido :: Estado -> Mov -> Bool
valido = \e m -> case m of
    AM -> validarAM e;
    AV -> validarAV e;
    SM -> validarSM e;
    SV -> validarSV e


mover :: Mov -> Estado ->  Estado
mover = \m e -> case valido e m  of
    True -> case m of 
        AM -> moverAM e;
        AV -> moverAV e;
        SV -> moverSV e;
        SM -> moverSM e;
    False -> e

{-Recibe una lista de movimientos para los cuales es valido realizar un
  movimiento (los movimientos validos se generan mediante filter) y 
  retorna el estado si se aplicara el moviiento a cada una de
  ellas en forma de lista con tuplas (mov,estado)-}

movimientosAux :: [Mov] -> Estado -> [(Mov,Estado)]
movimientosAux = \m e -> case m of
    [] -> [];
    x:sx -> [(x, mover x e)] ++ movimientosAux sx e


movimientos :: Estado -> [(Mov, Estado)]
movimientos = \e -> movimientosAux (filter (valido  e) [AM,AV,SM,SV]) e 

gano :: Estado -> Bool
gano = \e -> e == final
    

perdio :: Estado -> Bool
perdio = \e -> not (validarSV e || validarSM e ||
                    validarAM e || validarAV e || 
                    gano e)
