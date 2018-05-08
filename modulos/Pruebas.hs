module Modulos.Pruebas where

import System.Random
import Modulos.Tablero
import Modulos.Pieza

--Pruebas para Modulos.Piezas
piezas_posiblesMovimientos = posiblesMovimientos (Peon, N) ('B', 2)

piezas_posicionValida = error ""

--Prubas para Modulos.Tablero
tablero_dibujaTablero = putStrLn $ dibujaTablero creaTableroInicial tablero_dropInalcanzables

tablero_muevePieza = error ""

tablero_dropInalcanzables = dropInalcanzables creaTableroInicial (Peon, N) ('B', 5) $ posiblesMovimientos (Peon, N) ('B', 5)
