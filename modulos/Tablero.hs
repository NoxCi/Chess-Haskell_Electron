module Modulos.Tablero where

import Data.Maybe
import Data.Map.Strict
import Data.List
import Modulos.Pieza
import Modulos.Excepcion


type Tablero = Map Char (Map Integer (Maybe Pieza))

--Nos da el trablero inicial
creaTableroInicial :: Tablero
creaTableroInicial = fromList [('A',fromList [(1,Just (Torre, N)),(2,Just (Caballo, N)),(3,Just (Alfil, N)),(4,Just (Rey, N)),(5,Just (Reina, N)),(6,Just (Alfil, N)),(7,Just (Caballo, N)),(8,Just (Torre, N))]),
                               ('B',fromList [(1,Just (Peon, N)),(2,Just (Peon, N)),(3,Just (Peon, N)),(4,Just (Peon, N)),(5,Just (Peon, N)),(6,Just (Peon, N)),(7,Just (Peon, N)),(8,Just (Peon, N))]),
                               ('D',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('E',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('C',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('F',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('G',fromList [(1,Just (Peon, B)),(2,Just (Peon, B)),(3,Just (Peon, B)),(4,Just (Peon, B)),(5,Just (Peon, B)),(6,Just (Peon, B)),(7,Just (Peon, B)),(8,Just (Peon, B))]),
                               ('H',fromList [(1,Just (Torre, B)),(2,Just (Caballo, B)),(3,Just (Alfil, B)),(4,Just (Rey, B)),(5,Just (Reina, B)),(6,Just (Alfil, B)),(7,Just (Caballo, B)),(8,Just (Torre, B))])]

--Crea una representacion en String del tablero
dibujaTablero :: Tablero -> String
dibujaTablero t = error ""

--Mueve una pieza desde una posicion dada a otra
--si no puede no hace nada
muevePieza :: Tablero -> Posicion -> Posicion -> (Maybe Pieza,Tablero, Maybe Excepcion)
muevePieza t p1@(c1,i1) p2@(c2,i2)
  | not $ posicionValida p2 = (Nothing , t, Just CoordenadaInexistente)
  | otherwise = let
    pieza  = getPieza t p1 --la pieza a mover
    pieza' = getPieza t p2 --ls posible pieza que este en la posicion final
    in case pieza of
      Nothing -> (Nothing,t, Just NoHayPieza) --no hacemos nada y decimos la causa
      otherwise -> if not $ movValido t (fromJust pieza) p1 p2 -- si el moviemiento es valido
        then (Nothing,t, Just MovInvalido) --no hacemos nada y decimos la causa
        else let
          t' = adjust (\_ -> (adjust (\_ -> Nothing) i1 (t!c1))) c1 t --quitamos la pieza de donde estaba
          r = (pieza',adjust (\_ -> (adjust (\_ -> pieza) i2 (t!c2))) c2 t', Nothing) --la movemos a la posicion dada
          in case pieza' of --vemos si hay alguna pieza en la posicion destino
            Nothing -> r --si no hay
            otherwise -> if color (fromJust pieza) == color (fromJust pieza') --si hay, vemos que no sea del mismo color
              then (Nothing, t , Just AutoAtaque)
              else r


--Nos dice si el movimiento de una pieza dada es valido
movValido :: Tablero -> Pieza -> Posicion -> Posicion -> Bool
movValido tab pieza p1 p2 = elem p2 $ dropInalcanzables tab pieza $ posiblesMovimientos pieza p1

--Dado posbibles movimeintos de una pieza quita aquellas que son inalcanzables dado un tablero.
dropInalcanzables :: Tablero -> Pieza -> [Posicion] -> [Posicion]
dropInalcanzables tab pieza l@(x:xs) = case  pieza of
  (Torre, _) -> let piezas = (Prelude.filter (hayPieza tab) l)
                    l1 = [p | p <- l, not$ inLeft p piezas]
                    l2 = [p | p <- l1, not$ inRight p piezas]
                    l3 = [p | p <- l2, not$ up p piezas]
                    l4 = [p | p <- l3, not$ dawn p piezas]
                in l4
  (Caballo, _) ->  [p | p <- l , not$ hayPieza tab p]
  (Alfil, _) -> error ""
  (Rey, _) -> [p | p <- l , not$ hayPieza tab p]
  (Reina, _) -> let piezas = (Prelude.filter (hayPieza tab) l)
                    l1 = [p | p <- l, not$ inLeft p piezas]
                    l2 = [p | p <- l1, not$ inRight p piezas]
                    l3 = [p | p <- l2, not$ up p piezas]
                    l4 = [p | p <- l3, not$ dawn p piezas]
                in error "Faltan las diagonales"
  (Peon, _) -> error ""
  where
    inLeft p@(c1, p1) xs = case xs of
      [] -> False
      ((c2,p2):xs) -> (c1 == c2 && p1 < p2) || inLeft p xs
    inRight p@(c1, p1) xs = case xs of
      [] -> False
      ((c2,p2):xs) -> (c1 == c2 && p2 > p1) || inRight p xs
    up p@(c1, p1) xs = case xs of
      [] -> False
      ((c2,p2):xs) -> (toInt c1 > toInt c2 && p2 == p1) || up p xs
    dawn p@(c1, p1) xs = case xs of
      [] -> False
      ((c2,p2):xs) -> (toInt c1 < toInt c2 && p2 == p1) || dawn p xs

--Posiblemente nos devuelva una pieza
getPieza :: Tablero -> Posicion -> Maybe Pieza
getPieza t (c,i) = (t ! c) ! i

--Nos dice si hay una pieza en la posicon dada
hayPieza :: Tablero -> Posicion -> Bool
hayPieza tab p =  Nothing /= getPieza tab p
