module Library where
import PdePreludat

type Comida = Persona -> Persona
type Gramos = Number


data Persona = Persona {
    nombre :: String,
    calorias :: Number,
    nutrientes :: [String]
} deriving (Show,Eq)

-- punto 1 a
sofia = Persona { nombre = "sofia" ,calorias=0,nutrientes= ["ensalada"]}

--punto 1 b
agregarNutriente :: String -> Persona -> Persona
agregarNutriente nutriente persona
    | elem nutriente (nutrientes persona) = persona
    | otherwise =  persona {nutrientes = (nutriente:).nutrientes $ persona}

-- Punto 2
tomate :: Comida
tomate persona = foldr agregarNutriente persona ["vitamina A"," vitamina C"] 

zanahoria :: Comida
zanahoria persona = foldr agregarNutriente persona ["vitamina A"," vitamina C","vitamina E","vitamina K"] 

carne :: Gramos -> Comida 
carne gramo persona = (modificarCalorias gramo) $ foldr agregarNutriente persona ["calcio","hierro"]
    where modificarCalorias gramos persona = persona{calorias = (24*).(gramos+).calorias $ persona}

--modificarCalorias ::  Number -> Persona -> Persona
--modificarCalorias gramos persona = persona{calorias = (24*).(gramos+).calorias $ persona}

pan :: Persona->Persona -> Persona
pan tipoPan persona = persona

blanco :: Persona -> Persona
blanco persona = persona { calorias = (265+).calorias $ persona}
