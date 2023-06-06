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
 | otherwise = unjugador {cantdinero = cantdinero unjugador - 100}
   
--al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad. 
--Ganar implica restar el precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades. 
   
precio :: Propiedad -> Int
precio propiedad = snd propiedad

subastar :: Propiedad -> Jugador -> Jugador
subastar propiedad jugador
 | tactica jugador == "Oferente singular"  || tactica jugador == "Accionista" = jugador {cantdinero = cantdinero jugador - precio propiedad, propiedades = propiedades jugador ++ [propiedad]}
 | otherwise = jugador

--suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida.
--Las propiedades baratas son aquellas cuyo precio es menor a $150.
cobrarAlquileres :: Jugador -> Jugador
cobrarAlquileres jugador = jugador {cantdinero = cantdinero jugador + gananciatotal jugador} 

esbarato :: Precio -> Bool
esbarato unprecio = unprecio <150

gananciatotal :: Jugador -> Precio
gananciatotal jugador =  propiedadesBaratas jugador * 10 + propiedadesCaras jugador * 20

propiedadesBaratas :: Jugador -> Precio
propiedadesBaratas jugador = length (filter esbarato (preciosPropiedades jugador))

propiedadesCaras :: Jugador -> Precio
propiedadesCaras jugador = length (filter (not . esbarato) (preciosPropiedades jugador))

preciosPropiedades :: Jugador -> [Precio]
preciosPropiedades jugador = map precio (propiedades jugador)



baltica :: Propiedad
baltica = ("Baltica", 80)

palermo :: Propiedad
palermo = ("Palermo", 300)

recoleta :: Propiedad
recoleta = ("Recoleta", 350)

belgrano :: Propiedad
belgrano = ("Belgrano", 400)

nico :: Jugador
nico = Unjugador {nombre = "Nicolas", 
cantdinero = 500, tactica = "Oferente singular", 
propiedades = [baltica,palermo], acciones = [pasarporelbanco]}

cheto :: Jugador
cheto = Unjugador {nombre= "Cheto",
cantdinero = 500, tactica = "Accionista", 
propiedades = [recoleta,palermo,belgrano], acciones = [pasarporelbanco]}

tontaina :: Jugador
tontaina = Unjugador {nombre= "Tontaina",
cantdinero = 500, tactica = "nada", 
propiedades = [], acciones = [pasarporelbanco]}