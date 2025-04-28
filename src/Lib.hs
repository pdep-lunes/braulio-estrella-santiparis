module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {nombre :: String, poderBasico :: String, superPoder :: String, superPoderActivo :: Bool, vida :: Float}

restarVida :: Float -> Float -> Float
restarVida vidaContrincante vidaARestar 
  | vidaARestar >= vidaContrincante = 0
  | otherwise = vidaContrincante - vidaARestar 

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = UnPersonaje (nombre contrincante) (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (restarVida (vida contrincante) 1000)

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoAtaque otroPersonaje 
  | tipoAtaque == "sanadoras" = UnPersonaje (nombre otroPersonaje) (poderBasico otroPersonaje) (superPoder otroPersonaje) (superPoderActivo otroPersonaje) (vida otroPersonaje + 800)
  | tipoAtaque == "daninas" = UnPersonaje (nombre otroPersonaje) (poderBasico otroPersonaje) (superPoder otroPersonaje) (superPoderActivo otroPersonaje) (vida otroPersonaje / 2)
  | otherwise = otroPersonaje

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio contrincante 
  | radio > 3 && vida contrincante < 800 = UnPersonaje (nombre contrincante ++ " Espina estuvo aqui") (poderBasico contrincante) (superPoder contrincante) False 0
  | radio > 3 = UnPersonaje (nombre contrincante ++ " Espina estuvo aqui") (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (vida contrincante)
  | otherwise = bolaEspinosa contrincante

torretaCurativa :: Personaje -> Personaje
torretaCurativa aliado = UnPersonaje (nombre aliado) (poderBasico aliado) (superPoder aliado) True (vida aliado * 2)
