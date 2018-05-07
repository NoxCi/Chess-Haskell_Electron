module Modulos.Pruebas where

import System.Random
import Modulos.Tablero
import Modulos.Pieza

--Pruebas para Modulos.Piezas
piezas_posiblesMovimientos = posiblesMovimientos (Peon, N) ('B', 2)
piezas_posicionValida = error ""

--Prubas para Modulos.Tablero
tablero_dibujaTablero = putStrLn $ dibujaTablero creaTableroInicial []
tablero_muevePieza = case muevePieza creaTableroInicial ('G',5) ('F', 5) B of
  (pieza', r , Nothing) -> putStrLn $ dibujaTablero creaTableroInicial [] ++ "\n" ++ dibujaTablero r []
  (_,_, Just err) -> putStrLn $ dibujaTablero creaTableroInicial [] ++ "\n" ++ show err
tablero_dropInalcanzables = dropInalcanzables creaTableroInicial (Peon, N) ('F', 2) $ posiblesMovimientos (Peon, N) ('F', 2)
