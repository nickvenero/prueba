import Text.Show.Functions ()
{--
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc" --}

type Propiedad = (Nombre,Precio)
type Nombre = String
type Precio = Int
type Accion = Jugador -> Jugador

data Jugador = Unjugador { 
nombre :: Nombre, 
cantdinero :: Precio, 
tactica :: String, 
propiedades :: [Propiedad], 
acciones :: [Accion] 
} deriving Show

pasarporelbanco :: Jugador -> Jugador
pasarporelbanco jugador = jugador {cantdinero = cantdinero jugador + 40, tactica = "Comprador compulsivo"}

gritar :: Jugador -> Jugador
gritar jugador = jugador {nombre = "Ahhhhh" ++ (nombre jugador) }

enojarse :: Jugador -> Jugador
enojarse jugador = jugador {cantdinero = cantdinero jugador + 50, acciones = (acciones jugador) ++ [gritar]  }

carolina :: Jugador
carolina = Unjugador {nombre = "Carolina", 
cantdinero = 500, tactica = "Accionista", 
propiedades = [], acciones = [pasarporelbanco, 
pagarAccionistas]}

--Manuel es un “Oferente singular” y su acción inicial, además de la del banco, es enojarse.
manuel :: Jugador
manuel = Unjugador {nombre = "Manuel", 
cantdinero = 500, tactica = "Oferente singular", 
propiedades = [], acciones = [pasarporelbanco, 
enojarse]}


pagarAccionistas :: Jugador -> Jugador
pagarAccionistas unjugador
    | tactica unjugador == "Accionista" = unjugador {cantdinero = cantdinero unjugador + 200}
    | otherwise                         = unjugador {cantdinero = cantdinero unjugador - 100}


