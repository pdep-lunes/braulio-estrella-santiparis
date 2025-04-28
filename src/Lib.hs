module Lib () where

import Text.Show.Functions ()

data Personaje = UnPersonaje {nombre :: String, poderBasico :: String, superPoder :: String, superPoderActivo :: Bool, vida :: Int}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa contrincante = UnPersonaje (nombre contrincante) (poderBasico contrincante) (superPoder contrincante) (superPoderActivo contrincante) (vida contrincante - 1000)
