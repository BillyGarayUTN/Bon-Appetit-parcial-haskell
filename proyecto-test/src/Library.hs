module Library where
import PdePreludat

type Comida = Persona -> Persona
type Gramos = Number


data Persona = Persona {

    calorias :: Number,
    nutrientes :: [String]
} deriving (Show,Eq)

-- punto 1 a
sofia = Persona { calorias=0,nutrientes= ["ensalada"]}

--punto 1 b
agregarNutriente :: String -> Persona -> Persona
agregarNutriente nutriente persona
    | nutriente `elem` nutrientes persona = persona
    | otherwise =  persona {nutrientes = (nutriente:).nutrientes $ persona}

-- Punto 2
tomate :: Comida
tomate persona = foldr agregarNutriente persona ["vitamina A"," vitamina C"]

tomate' :: Persona -> Persona
tomate'  = vitC.vitA

vitA :: Persona -> Persona
vitA  = agregarNutriente "vitamina A"

vitC :: Persona -> Persona
vitC  = agregarNutriente "vitamina C"

zanahoria :: Comida
zanahoria persona = foldr agregarNutriente persona ["vitamina A"," vitamina C","vitamina E","vitamina K"]

carne :: Gramos -> Comida
carne gramo persona = modificarCalorias gramo $ foldr agregarNutriente persona ["calcio","hierro"]
    where modificarCalorias gramos persona = persona{calorias = (24*).(gramos+).calorias $ persona}

carne' :: Number -> Persona -> Persona
carne' gramos  = modificarCalorias' gramos.vitA

modificarCalorias' ::  Number -> Persona -> Persona
modificarCalorias' gramos persona = persona{calorias = (24*).(gramos+).calorias $ persona}

pan :: (Persona->Persona) -> Comida
pan tipoPan persona = tipoPan $ foldr agregarNutriente persona ["zinc"]

pan' :: (Persona -> Persona) -> Comida
pan' tipoPan  = tipoPan.vitA

--tipos de pan
blanco :: Persona -> Persona
blanco persona = persona { calorias = (265+).calorias $ persona}

integral :: Persona -> Persona
integral persona = persona {nutrientes = ("fibra":).nutrientes $ persona, calorias = (200+).calorias $ persona}

papa :: Persona -> Persona
papa persona
    | (2000<).calorias $ persona = persona { calorias = (100+).calorias $ persona}
    | otherwise = persona { calorias = (500+).calorias $ persona}

hanburguesaCheta :: Comida
hanburguesaCheta persona = foldr ($) persona  [pan papa,tomate,carne 180,pan papa]

hanburguesaCheta' :: Persona -> Persona
hanburguesaCheta'  = pan papa.carne 180 .tomate.pan papa

comeMenu ::[Persona->Persona] ->Persona -> Persona
comeMenu menu persona = foldr ($) persona menu

data Evento = Evento{
    nombre :: String,
    menu :: Persona->Persona,
    invitados :: [Persona]
} deriving (Show,Eq)

altaFiesta :: [Persona] -> Bool
altaFiesta  = all satisfecho
    where satisfecho persona = (pipona persona ||) $ cincoNutriente persona
--pipona persona || cincoNutriente persona 

pipona :: Persona -> Bool
pipona  = (2000<).calorias

cincoNutriente :: Persona-> Bool
cincoNutriente  = (5<).length.nutrientes

listaInvitados = [sofia]

