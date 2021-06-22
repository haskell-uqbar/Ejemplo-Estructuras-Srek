module Srek where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

siguiente x = x + 1

cuentaLoca x = x*x + x + 1

noTanBasica x n = x (n+1)

flip' f p1 p2 = f p2 p1


-- Burro
-- Jenjibre
-- fiona
-- fabiani


-- ogros
-- nombre
-- fealdad nro
-- cualidades ( )
-- hechizado 

-- data Bool 

data Ogro  =  UnOgro {
    nombre :: String,
    fealdad :: Number,
    cualidades :: [String],
    hechizado :: Bool
} deriving Show

data Pantano = UnPantano {
    ubicacion :: String,
    temperatura :: Number,
    ogros:: [Ogro]
}deriving Show

unOgroDeEjemplo = UnOgro "fabiani" 86 ["alto", "juegaBien"] False
shrek = UnOgro "Shrek" 100 ["amable","gruñon","lento","alegre"] False
fiona = UnOgro "Fiona" 50 ["buena","valiente","astuta" ]  True
 
elPantanoDeShrek = UnPantano "neverland" 200 [unOgroDeEjemplo, UnOgro "Shrek" 10 [] True]


buenPantano :: Pantano -> Bool

buenPantano (UnPantano "bueno" t []) = True
buenPantano (UnPantano "bueno" t ogros) = False
buenPantano (UnPantano nombre 200 ogros ) = True
buenPantano (UnPantano nombre t [] ) = t > 100
buenPantano otro = False



promedioFealdad :: Pantano -> Number
promedioFealdad pantano = fealdadTotal pantano /cantidadOgros pantano

cantidadOgros pantano = length (ogros pantano)

fealdadTotal pantano = sumarFealdad (ogros pantano)

sumarFealdad :: [Ogro] -> Number
--sumarFealdad  [] = 0
--sumarFealdad (ogro:ogros) = fealdad ogro   + sumarFealdad ogros  
sumarFealdad ogros = sum (map fealdad ogros )

length' [] = 0
length' (x:xs) = 1 + length' xs

esCero 0 = True
esCero n = False

y True True = True
y a b = False





esMuyFeo :: Ogro -> Bool
esMuyFeo ogro = fealdad ogro > 100

sauna :: Number -> Ogro -> Ogro
sauna temperatura (UnOgro nom fea c hech) = UnOgro nom (fea + temperatura/100 ) c hech 

hechizar:: Ogro -> Ogro
hechizar (UnOgro nom fea c hech) = UnOgro nom fea c True

presentacion :: Ogro -> String
presentacion (UnOgro nom fea c hech )
    | hech = "El gran " ++ nom ++  " que es un galan"
    | otherwise = "El ogro " ++ nom ++ " " ++ show (fea * 2)

presentacion2 :: Ogro -> String
presentacion2 ogro
    | hechizado ogro = "El gran " ++ nombre ogro  ++  " que es un galan"
    | otherwise = "El ogro " ++ nombre ogro ++ " " ++ show (fealdad ogro * 2)



-- Un Ogro se queda en cualquier pantano si no es muy feo
--seQuedaEnPantano :: Ogro -> Bool
--seQuedaEnPantano ogro =  not (esMuyFeo ogro)

quedarseEnPantano :: Ogro -> Pantano -> Pantano
quedarseEnPantano ogro pantano
    | esMuyFeo ogro || pantanoLleno pantano = pantano
    | otherwise = agregarOgro (sauna (temperatura pantano) ogro) pantano

agregarOgro :: Ogro -> Pantano -> Pantano
agregarOgro ogro (UnPantano n t ogros) = UnPantano n t (ogro:ogros)


--familia de shrek y fiona
--tienen ogritos ¿como son?
--uno se parece al padre otro a la madre

ogritos:: Ogro -> Ogro -> Number -> [Ogro]
ogritos ogro otroOgro 3 = [UnOgro (nombre ogro ++ " Jr.") 0 [] False ,
                         UnOgro (take 5 (nombre otroOgro)) 10 (cualidades otroOgro) False,
                         UnOgro "cachito" (max (fealdad otroOgro) (fealdad ogro)) (take 2 (cualidades ogro)++(drop 2 (cualidades otroOgro))) (hechizado ogro || hechizado otroOgro) ]

ogritos ogro otroOgro 0 = []
ogritos ogro otroOgro 1 = [unOgroDeEjemplo]     
ogritos ogro otroOgro n =  []   


--el pantano esta lleno si hay mas de 3 ogros
pantanoLleno pantano = length (ogros pantano) > 3


--premios segun la situacion
-- si tiene como cualidad "jugarBien" -> futbolista
-- si es vacia la lista de cualidades -> insulso


--type Premio = String
--futbolista = "Premio al futbolista"

-- type Premio = Number
--futbolista = 1000

futbolista = UnPremio "Premio al futbolista" 1000
sinPremio =futbolista
insulso = futbolista
premioFeo = futbolista


data Premio = UnPremio {
    descripcion::String,
    importe::Number
} deriving Show

futbolista, insulso, premioFeo, sinPremio:: Premio

premio:: Ogro -> Premio
premio ogro
    | tieneCualidad ogro "jugar bien" = futbolista
    | sinCualidades ogro = insulso
    | esMuyFeo ogro = premioFeo
    | otherwise = sinPremio


tieneCualidad ogro cualidad = elem cualidad (cualidades ogro)
sinCualidades ogro= null (cualidades ogro)
