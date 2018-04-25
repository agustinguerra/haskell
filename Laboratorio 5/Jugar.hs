module Jugar where 

import Ranas
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT, gets, modify)

data MovAux = R Mov | H | Q deriving (Eq)

read :: String -> MovAux
read "SV" = R SV
read "AV" = R AV
read "SM" = R SM
read "AM" = R AM
read "Q" = Q
read "H" = H

validMov :: String -> Bool
validMov "SV" = True
validMov "AV" = True
validMov "SM" = True
validMov "AM" = True
validMov "Q" = True
validMov "H" = True
validMov _ = False

obtenerMov :: MovAux -> Mov
obtenerMov (R SV) = SV
obtenerMov (R AV) = AV
obtenerMov (R SM) = SM
obtenerMov (R AM )= AM

movInvalido :: Mov -> Estado -> Bool
movInvalido m e = not (valido e m )

jugar :: IO ()
jugar = evalStateT loop inicial
  where

    loop :: StateT Estado IO ()
    loop = do
      impr
      mov <- lift siguienteMov
      estado <- get   
      if mov == H then do
        lift $ putStrLn $ "Movimientos posibles: " ++ show (map fst (movimientos estado))
        loop
        else if mov == Q then do
            return ()
            else if movInvalido (obtenerMov mov) estado then do
              lift $ putStrLn $ "Movimiento inválido, los movimientos posibles son: " ++ show (map fst (movimientos estado))
              loop
              else do
                Control.Monad.Trans.State.modify (mover (obtenerMov mov))
                ganaste <- Control.Monad.Trans.State.gets gano
                perdiste <- Control.Monad.Trans.State.gets perdio
                if ganaste then do lift $ putStrLn  $ "FELICITACIONES GANASTE!"
                  else if perdiste then do lift $ putStrLn $ "GAME OVER - No hay más movimientos posibles: " ++  (mostrar estado)
                    else loop
 
    impr :: StateT Estado IO ()
    impr = do
      lift $ putStrLn $ "El estado es el siguiente:"
      estado <- get
      lift $ putStrLn $ mostrar estado
      
    siguienteMov :: IO MovAux
    siguienteMov = do
      putStr "Indique un movimiento, o tipee H (ayuda) o Q (salir): "
      mov <- getLine
      if validMov mov
        then return $ Jugar.read mov
            else do
              putStrLn "Movimiento invalido."
              siguienteMov
              