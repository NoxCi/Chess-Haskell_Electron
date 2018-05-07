module Modulos.Pruebas where

import System.Random
import Modulos.Tablero
import Modulos.Pieza

--Pruebas para Modulos.Piezas
piezas_posiblesMovimientos = posiblesMovimientos (Reina, B) ('D', 5)
piezas_posicionValida = error ""

--Prubas para Modulos.Tablero
tablero_dibujaTablero = putStrLn $ dibujaTablero creaTableroInicial $ tablero_dropInalcanzables
tablero_dropInalcanzables = dropInalcanzables creaTableroInicial (Caballo, B) ('D', 5) $ posiblesMovimientos (Caballo, B) ('D', 5)
