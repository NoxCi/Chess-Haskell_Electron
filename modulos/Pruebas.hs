module Modulos.Pruebas where

import System.Random
import Modulos.Tablero
import Modulos.Pieza

piezas_posiblesMovimientos = do
  gen <- newStdGen
  let (rTipo, newGen) = randomR (1, 6) gen :: (Int, StdGen)
      (rColor, newGen') = randomR (1, 2) newGen :: (Int, StdGen)
      (rC , newGen'') = randomR (1,8) newGen' :: (Int, StdGen)
      (rI , newGen''') = randomR (1,8) newGen'' :: (Int, StdGen)
      tipo = case rTipo of
         1 -> Torre
         2 -> Caballo
         3 -> Alfil
         4 -> Rey
         5 -> Reina
         6 -> Peon
      color = case rColor of
        1 -> B
        2 -> N
      posicion = makePosicion ((read . show) rC, (read . show) rI)
  putStrLn (show (tipo,color) ++ " " ++ show posicion ++ ":\n "++ show  (posiblesMovimientos (tipo, color) posicion))

piezas_dropInalcanzables = dropInalcanzables creaTableroInicial (Torre, B) $ posiblesMovimientos (Torre, B) ('H', 8)

piezas_posicionValida = posicionValida ('C', 0)
