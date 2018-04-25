--
-- Autor(es): Mariano Zunino - Agustin Guerra
-- Numero(s) de estudiante(s): 212686 - 196344
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Prog where

-- Tipos
type Var = String

data Exp where {
    V    :: Var -> Exp;
    N  :: Int -> Exp;
    (:+)  :: Exp -> Exp -> Exp;
    (:-)  :: Exp -> Exp -> Exp;
    (:*) :: Exp -> Exp -> Exp  }
  deriving(Eq,Show)

-- Se define que :* tiene mayor precedencia que :+ y :-
infixl 6 :+
infixl 6 :-
infixl 8 :*

type Memoria = [(Var,Int)]

data Prog where {
    (:=)  :: Var -> Exp -> Prog;
    (:>)  :: Prog -> Prog -> Prog;
    If    :: Exp -> Prog -> Prog -> Prog;
    While :: Exp -> Prog -> Prog  }
  deriving(Eq,Show)

-- Se define que := tiene mayor precedencia que :>
infixr 5 :=
infixr 3 :>

-- 1
{- Filter recibe como parametro una exp lambda, que permite
  filtrar las tuplas donde el primer valor sea igual que el de v,
  si no existe devuelve mensaje de error-}

(@@) :: Var -> Memoria -> Int
(@@) = \ v m -> case filter (\(z,_) -> z == v) m of {
    [] -> error "Variable no inicializada";
    x:_ -> case x of (a,b) -> b;
}

-- 2
{- Filter recibe como parametro una exp lambda, que me permite filtrar
 la memoria, devolviendo toda la memoria sin incluir la variable
 que se desea actualizar, y la agregar al final de la nueva lista -}

upd :: (Var, Int) -> Memoria -> Memoria
upd = \v m -> case v of (a,_) -> v:(filter (\(z,_) -> z /= a) m)

final :: Memoria
final = [("A",5),("B",2),("C",6)]

-- 3
eval :: Exp -> Memoria -> Int
eval = \e m -> case e of {
    N b -> b;
    V x -> x @@ m;
    e1 :+ e2 -> (eval e1 m) + (eval e2 m);
    e1 :- e2 -> (eval e1 m) - (eval e2 m);
    e1 :* e2 -> (eval e1 m) * (eval e2 m)
}

-- 4
run :: Prog -> Memoria -> Memoria
run = \p m -> case p of {
    If e1 p1 p2 -> case (eval e1 m) == 0 of{
        True -> run p2 m;
        False -> run p1 m
    };
    While e1 p1 -> case (eval e1 m) /= 0 of {
        True -> run (While e1 p1) (run p1 m);
        False -> m;
    };
    v1 := e1 -> upd (v1,(eval e1 m)) m ;
    p1 :> p2 -> (run p2 (run p1 m))
}

-- Ejemplos
p0 :: Prog
p0 = "x" := N 1 :> "x" :=  V "x" :+ N 10

p1 :: Prog
p1 =  "x" := N 1 :> "y" := N 2 :>
      If  (V "y" :- V "x")
      ("z" := N 10)
      ("z" := N 20)

p2 :: Prog
p2 = "x" := N 10 :> "y" := N 5 :>
     While (V "x") ( "y" := V "y" :+ N 2 :> "x" :=  V "x" :-N 1)

-- 5
swap:: Prog
swap = "aux" := V "y" :> "y" := V "x" :> "x" := V "aux" 

-- 6
{- Resultado esta inicializado en 1, por propiedad de Fact. 
 Mientras N sea diferente de cero, actualizar resultado multiplicado por N,
 despues restar a n 1.
 El programa no admite numeros negativos, porque de la forma que se definio el 
 while nunca alcanzaria 0 para que se detenga.
-}

fact :: Int -> Prog
fact = \i -> case i >= 0 of 
    True -> "n" := N i :> "resultado" := N 1 :>
        While (V "n") ( "resultado" := V "resultado" :* V "n" :> "n" :=  V "n" :-N 1);
    False -> error "El numero no puede ser negativo"


