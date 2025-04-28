module Lib () where

import Text.Show.Functions ()

type Poder = Personaje -> Personaje

data Personaje = UnPersonaje {nombre :: String, poderBasico :: Poder, superPoder :: Poder, superPoderActivo :: Bool, vida :: Int} deriving (Show)

type Partida = [Personaje]

restarVida :: Int -> Int -> Int
restarVida vidaContrincante vidaARestar
  | vidaARestar >= vidaContrincante = 0
  | otherwise = vidaContrincante - vidaARestar

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio contrincante danio = contrincante {vida = restarVida (vida contrincante) danio}

curarVida :: Personaje -> Int -> Personaje
curarVida aliado aCurar = aliado {vida = vida aliado + aCurar}

cambiarNombre :: Personaje -> String -> Personaje
cambiarNombre personaje aAgregar = personaje {nombre = nombre personaje ++ aAgregar}

desactivarSuperPoder :: Personaje -> Personaje
desactivarSuperPoder personaje = personaje {superPoderActivo = False}

activarSuperPoder :: Personaje -> Personaje
activarSuperPoder personaje = personaje {superPoderActivo = True}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = hacerDanio contrincante 1000

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoAtaque otroPersonaje
  | tipoAtaque == "sanadoras" = curarVida otroPersonaje 800
  | tipoAtaque == "daninas" = hacerDanio otroPersonaje (div (vida otroPersonaje) 2)
  | otherwise = otroPersonaje

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio contrincante
  | radio > 3 && vida contrincante < 800 = cambiarNombre (hacerDanio (desactivarSuperPoder contrincante) (vida contrincante)) " Espina estuvo aqui"
  | radio > 3 = bolaEspinosa.cambiarNombre contrincante $ " Espina estuvo aqui"
  | otherwise = bolaEspinosa contrincante

torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = activarSuperPoder.curarVida aliado $ vida aliado

estaEnLasUltimas :: Personaje -> Bool
estaEnLasUltimas = (< 800).vida

personajesEnLasUltimas :: Partida -> [String]
personajesEnLasUltimas = map nombre.filter estaEnLasUltimas

atacarConPoderEspecial :: Personaje -> Personaje -> Personaje
atacarConPoderEspecial atacante objetivo 
  | superPoderActivo atacante = poderBasico atacante.superPoder atacante $ objetivo
  | otherwise = objetivo

espina :: Personaje
espina = UnPersonaje "Espina" bolaEspinosa (granadaDeEspinas 5) True 4800 

pamela :: Personaje
pamela = UnPersonaje "Pamela" (lluviaDeTuercas "sanadoras") torretaCurativa False 9600

partida :: Partida
partida = [espina, pamela] :: Partida