{-# LANGUAGE NPlusKPatterns #-}
module Listas where
import Prelude hiding (all, and, any, concat, filter, length, map,
                      null, or, reverse, sum, zip, zipWith, (++))
pruebaUno::[Int]
pruebaUno=[11,24,213,40]

pruebaDos::[Bool]
pruebaDos=[True,False,False,False]

null::[a] -> Bool
null l = case l of
    []  -> True;
    _:_ ->False

length :: [a] -> Int
length l = case l of
    []   -> 0;
    _:sx -> 1+length sx

duplicate :: [a] -> [a]
duplicate l = case l of
    []   -> [];
    x:sx -> x:x:duplicate sx

sum :: [Int] -> Int
sum l = case l of
    []   -> 0;
    x:sx -> x+sum sx

prod :: [Int] -> Int
prod l = case l of
    []   -> 1;
    x:sx -> x* prod sx

map::(a->b) ->[a]-> [b]
map f l = case l of
    []   -> [];
    x:sx -> f x: map f sx

zip::[a]-> [b]-> [(a,b)]
zip l p = case l of
    [] -> [];
    x:sx -> case p of
        []   -> [];
        y:sy -> (x,y):zip sx sy

zipWith::(a->b->c)-> [a]-> [b]-> [c]
zipWith f l p = case l of
    [] -> [];
    x:sx -> case p of {
        []   -> [];
        y:sy -> f x y : zipWith f sx sy
    }

filter::(a->Bool) ->[a]-> [a]
filter f l = case l of
    [] -> [];
    x:sx -> case f x of {
        False -> [];
        True  -> x:filter f sx
    }

and::[Bool]-> Bool
and l = case l of
    []   -> True;
    x:sx -> x && and sx

or::[Bool]-> Bool
or l = case l of
    []   -> False;
    x:sx -> x || or sx

cuantos::(a->Bool) ->[a]-> Int
cuantos f l = case l of
    [] -> 0;
    x:sx -> case f x of {
        True  -> 1+cuantos f sx;
        False -> cuantos f sx
    }

any::(a->Bool) ->[a]->Bool
any f l = case l of
    []   -> False;
    x:sx -> f x || any f sx

all::(a->Bool) ->[a]->Bool
all f l = case l of
    [] -> True;
    x:sx -> case f x of {
        True  -> all f sx;
        False -> False
    }

(++)::[a]-> [a]-> [a]
(++) l p = case l of
    []   -> p;
    x:sx ->  x:(sx ++ p)

reverse:: [a]-> [a]
reverse l = case l of
    []   -> [];
    x:sx -> reverse sx ++ [x]

concat::[[a]]-> [a]
concat l = case l of
    [] -> [];
    x:sx -> case x of
        []   -> concat sx;
        y:sy -> [y] ++ sy ++ concat sx

lensum::[[a]] -> Int
lensum l = case l of
    []   -> 0;
    x:sx -> length x + lensum sx
