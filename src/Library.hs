module Library where
import PdePreludat
   

type Nombre = String 
type Edad = Number
type Fama = Number
type Pack = (Nombre,Number)


data Persona = UnPersona{
    nombre :: Nombre,
    edad :: Edad,
    ciudad :: Nombre,
    esFutbolista :: Bool,
    pack :: Pack
}  deriving (Show, Eq)

mirtha :: Persona
mirtha = UnPersona{
    nombre = "Mirta Legrand",
    edad = 98,
    ciudad = "Mar del Plata",
    esFutbolista = False,
    pack = ("Prendas de ropa",3)
}

momo :: Persona
momo = UnPersona "Momo" (36 + 1) "CABA" False ("Productos italianos",10)

rodrigo :: Persona
rodrigo = UnPersona{
    nombre = "Rodrigo De Paul",
    edad = 30,
    esFutbolista = True,
    ciudad = "Madrid",
    pack = ("Articulos de futbol",3) 
}

tini :: Persona
tini = UnPersona{
    nombre = "Martina Stoessel",
    edad = 28,
    esFutbolista = False,
    ciudad = "CABA",
    pack = ("Articulos de moda",6) 
}


leandro :: Persona
leandro = UnPersona{
    nombre = "Leandro Paredes",
    edad = 30,
    esFutbolista = True,
    ciudad = "Roma",
    pack = ("Articulos de futbol",4) 
}

messi :: Persona
messi = UnPersona{
    nombre = "Lionel Andres Messi",
    edad = 37,
    esFutbolista = True,
    ciudad = "Miami",
    pack = ("Articulos de futbol",16) 
}


sonAmigos :: Persona -> Persona -> Bool
sonAmigos persona1 persona2 = edad persona1 == edad persona2 

nivelDeFama :: Persona -> Fama
nivelDeFama persona = length(nombre persona) + edad persona 

diferenciadeEdad :: Persona -> Persona -> Edad
diferenciadeEdad persona1 persona2 = abs(edad persona1 - edad persona2)

compatibleCon :: Persona -> Persona -> Bool
compatibleCon persona1 persona2 = diferenciadeEdad persona1 persona2 <= 3 && nivelDeFama persona1 >= 40 && nivelDeFama persona2 >= 40

{- otra posible forma de resolver este punto 
nivelDeVendeHumo :: Persona -> Number
nivelDeVendeHumo persona 
    |nombre persona == "Lionel Andres Messi" = 0
    |esPortenio (ciudad persona) = sumaVendeHumo persona + 100
    |otherwise = sumaVendeHumo persona
-}

nivelDeVendeHumo :: Persona -> Number
nivelDeVendeHumo (UnPersona "Lionel Andres Messi" edad ciudad fut pack) = 0
nivelDeVendeHumo (UnPersona nom edad "CABA" fut pack)
 = sumaVendeHumo (UnPersona nom edad "CABA" fut pack) + 100
nivelDeVendeHumo persona = sumaVendeHumo persona

esPortenio :: Nombre -> Bool
esPortenio "CABA" = True
esPortenio ciudad = False

sumaVendeHumo :: Persona -> Number
sumaVendeHumo persona = cantidadObjeto (pack persona) * 10

cantidadObjeto :: Pack -> Number
--cantidadObjeto pack = snd pack 
cantidadObjeto (descripcion,cantidad) = cantidad

nuevoNombre::Nombre->Nombre
nuevoNombre nombre = nombre ++ "EEEEEEHHH"

modoDiego::Persona ->Persona
modoDiego (UnPersona nom edad ciu fut pack) = UnPersona (nom ++ "EEEEEEHHH") edad ciu fut pack

mudanza :: Persona -> Nombre -> Persona
mudanza (UnPersona nom edad ciudad fut pack) nuevaCiudad = UnPersona nom edad nuevaCiudad fut pack