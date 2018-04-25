module Lab1 where
import Prelude (Show)
id :: a -> a
id = \x -> x
--comentario
kid :: a-> b -> b
kid = \x -> \y -> y

k :: a -> b -> a
k = \x -> \y -> x

apply :: (a -> b ) -> a -> b
apply = \x -> \y -> x y

twice :: (a -> a ) -> a -> a
twice = \x -> \x -> x 

flip :: (a -> b -> c) -> b -> a -> c
flip = \x -> \y -> \z -> x z y

(.) :: (b ->c) -> (a->b) -> a -> c
(.) = \f -> \g -> \x -> f (g x)  

h1 :: (a -> b -> c) -> a -> b -> c
h1 = \x -> \y -> \z -> x y z