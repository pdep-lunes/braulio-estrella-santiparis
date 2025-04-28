module Lib () where

import Text.Show.Functions ()
import Foreign (alignPtr)

data Personaje = UnPersonaje {nombre :: String, poderBasico :: Personaje -> Personaje, superPoder :: Personaje -> Personaje, superPoderActivo :: Bool, vida :: Int} deriving (Show)

type Partida = [Personaje]

restarVida :: Int -> Int -> Int
restarVida vidaContrincante vidaARestar
  | vidaARestar >= vidaContrincante = 0
  | otherwise = vidaContrincante - vidaARestar

hacerDanio :: Personaje -> Int -> Personaje
hacerDanio contrincante danio = UnPersonaje (nombre contrincante) (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (restarVida (vida contrincante) danio)

curarVida :: Personaje -> Int -> Personaje
curarVida aliado aCurar = UnPersonaje (nombre aliado) (poderBasico aliado) (superPoder aliado) (superPoderActivo aliado) (vida aliado + aCurar)

cambiarNombre :: Personaje -> String -> Personaje
cambiarNombre personaje aAgregar = UnPersonaje (nombre personaje ++ aAgregar) (poderBasico personaje) (superPoder personaje) (superPoderActivo personaje) (vida personaje)

desactivarSuperPoder :: Personaje -> Personaje
desactivarSuperPoder personaje = UnPersonaje (nombre personaje) (poderBasico personaje) (superPoder personaje) False (vida personaje)

activarSuperPoder :: Personaje -> Personaje
activarSuperPoder personaje = UnPersonaje (nombre personaje) (poderBasico personaje) (superPoder personaje) True (vida personaje)

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
  | radio > 3 = cambiarNombre contrincante " Espina estuvo aqui"
  | otherwise = bolaEspinosa contrincante

torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = activarSuperPoder.curarVida aliado $ vida aliado