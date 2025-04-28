module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {nombre :: String, poderBasico :: String, superPoder :: String, superPoderActivo :: Bool, vida :: Float}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = UnPersonaje (nombre contrincante) (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (vida contrincante - 1000)

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas tipoAtaque contraparte 
  | tipoAtaque == "sanadoras" = UnPersonaje (nombre contraparte) (poderBasico contraparte) (superPoder contraparte) (superPoderActivo contraparte) (vida contraparte + 800)
  | tipoAtaque == "daninas" = UnPersonaje (nombre contraparte) (poderBasico contraparte) (superPoder contraparte) (superPoderActivo contraparte) (vida contraparte / 2)
  | otherwise = contraparte

granadaDeEspinas :: Int -> Personaje -> Personaje
granadaDeEspinas radio contrincante 
  | radio > 3 && vida contrincante < 800 = UnPersonaje (nombre contrincante ++ " Espina estuvo aqui") (poderBasico contrincante) (superPoder contrincante) False 0
  | radio > 3 = UnPersonaje (nombre contrincante ++ " Espina estuvo aqui") (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (vida contrincante - 1000)
  | otherwise = bolaEspinosa contrincante
