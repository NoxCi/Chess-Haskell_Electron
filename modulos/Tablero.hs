module Modulos.Tablero where

import Data.Maybe
import Data.Map.Strict
import Data.List
import Modulos.Pieza

type Tablero = Map Char (Map Integer (Maybe Pieza))

--Nos da el trablero inicial
creaTableroInicial :: Tablero
creaTableroInicial = fromList [('A',fromList [(1,Just (Torre, N)),(2,Just (Caballo, N)),(3,Just (Alfil, N)),(4,Just (Rey, N)),(5,Just (Reina, N)),(6,Just (Alfil, N)),(7,Just (Caballo, N)),(8,Just (Torre, N))]),
                               ('B',fromList [(1,Just (Peon, N)),(2,Just (Peon, N)),(3,Just (Peon, N)),(4,Just (Peon, N)),(5,Just (Peon, N)),(6,Just (Peon, N)),(7,Just (Peon, N)),(8,Just (Peon, N))]),
                               ('C',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('D',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('E',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('F',fromList [(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing)]),
                               ('G',fromList [(1,Just (Peon, B)),(2,Just (Peon, B)),(3,Just (Peon, B)),(4,Just (Peon, B)),(5,Just (Peon, B)),(6,Just (Peon, B)),(7,Just (Peon, B)),(8,Just (Peon, B))]),
                               ('H',fromList [(1,Just (Torre, B)),(2,Just (Caballo, B)),(3,Just (Alfil, B)),(4,Just (Rey, B)),(5,Just (Reina, B)),(6,Just (Alfil, B)),(7,Just (Caballo, B)),(8,Just (Torre, B))])]

--Crea una representacion en String del tablero
dibujaTablero :: Tablero -> [Posicion] -> String
dibujaTablero t ls =  "     _____ _____ _____ _____ _____ _____ _____ _____\n"
                   ++ draw (toList t) 0 ls
  where
    draw [] _ _= "       1     2     3     4     5     6     7     8"
    draw ((c, mapa): rs) i ls = let
       ls' = [n | (c', n) <- ls, c' == c]
      in if even i
        then "    |     |-----|     |-----|     |-----|     |-----|\n"
          ++ show c ++ " |" ++ draw' (toList mapa) ls' ++ draw rs (i+1) ls
        else "    |-----|     |-----|     |-----|     |-----|     |\n"
           ++ show c ++ " |" ++ draw' (toList mapa) ls' ++ draw rs (i+1) ls

    draw' [] _ = "\n    |_____|_____|_____|_____|_____|_____|_____|_____|\n"
    draw' ((i, pieza): rs) ls = if elem i ls
      then case pieza of
        Nothing -> "|   ||" ++ draw' rs ls
        Just (t,color) -> "|" ++ show color ++ "," ++ show t ++ "||" ++ draw' rs ls
      else case pieza of
        Nothing -> "     |" ++ draw' rs ls
        Just (t,color) -> " " ++ show color ++ "," ++ show t ++ " |" ++ draw' rs ls

--Mueve una pieza desde una posicion dada a otra
--si no puede no hace nada
muevePieza :: Tablero -> Posicion -> Posicion -> Tablero
muevePieza t p@(c1,i1) (c2,i2) = let
      pieza  = getPieza t p
      t' = adjust (\_ -> (adjust (\_ -> Nothing) i1 (t!c1))) c1 t --quitamos la pieza de donde estaba
      r = adjust (\_ -> (adjust (\_ -> pieza) i2 (t'!c2))) c2 t' --la movemos a la posicion dada
      in r

--Dado posbibles movimeintos de una pieza quita aquellas que son inalcanzables dado un tablero.
dropInalcanzables :: Tablero -> Pieza -> Posicion -> [Posicion] -> [Posicion]
dropInalcanzables tab pieza pI ls = case  pieza of
  (Torre, c) -> let
    l1 = [p | p <- ls, not $ left p lefts]
    l2 = [p | p <- l1, not $ right p rights]
    l3 = [p | p <- l2, not $ up p ups]
    l4 = [p | p <- l3, not $ dawn p dawns]
    in Prelude.filter (distintoColor tab c) l4
  (Caballo, c) -> Prelude.filter (distintoColor tab c) ls
  (Alfil, c) -> let
    l1 = [p | p <- ls, not $ upLeft p upLefts]
    l2 = [p | p <- l1, not $ upRight p upRights]
    l3 = [p | p <- l2, not $ dawnLeft p dawnLefts]
    l4 = [p | p <- l3, not $ dawnRight p dawnRights]
    in Prelude.filter (distintoColor tab c) l4
  (Rey, c) -> Prelude.filter (distintoColor tab c) ls
  (Reina, c) -> let
    l1 = [p | p <- ls, not $ left p lefts]
    l2 = [p | p <- l1, not $ right p rights]
    l3 = [p | p <- l2, not $ up p ups]
    l4 = [p | p <- l3, not $ dawn p dawns]
    l5 = [p | p <- l4, not $ upLeft p upLefts]
    l6 = [p | p <- l5, not $ upRight p upRights]
    l7 = [p | p <- l6, not $ dawnLeft p dawnLefts]
    l8 = [p | p <- l7, not $ dawnRight p dawnRights]
    in Prelude.filter (distintoColor tab c) l8
  (Peon, N) -> let
    l1 = [p | p <- ls, not (dawnLeft p [pI]) || (dawnLeft p [pI] && hayPieza tab p)]
    l2 = [p | p <- l1, not (dawnRight p [pI]) || (dawnRight p [pI] && hayPieza tab p)]
    in Prelude.filter (distintoColor tab N) [p | p <- l2, not (dawn p [pI]) || (dawn p [pI] && not (hayPieza tab p))]

  (Peon, B) -> let
    l1 = [p | p <- ls, not (upLeft p [pI]) || (upLeft p [pI] && hayPieza tab p)]
    l2 = [p | p <- l1, not (upRight p [pI]) || (upRight p [pI] && hayPieza tab p)]
    in Prelude.filter (distintoColor tab B) [p | p <- l2, not (up p [pI]) || (up p [pI] && not (hayPieza tab p))]

  where
    piezas = Prelude.filter (hayPieza tab) ls

    distintoColor tab c x = case getPieza tab x of
      Nothing -> True
      Just (_,c') -> c /= c'

    lefts = [p | p <- piezas, left p [pI]]
    rights = [p | p <- piezas, right p [pI]]
    ups = [p | p <- piezas, up p [pI]]
    dawns = [p | p <- piezas, dawn p [pI]]

    upLefts = [p | p <- piezas, upLeft p [pI]]
    upRights = [p | p <- piezas, upRight p [pI]]
    dawnLefts = [p | p <- piezas, dawnLeft p [pI]]
    dawnRights = [p | p <- piezas, dawnRight p [pI]]

    --Todas la siguientes funciones nos dicen dada una posicon y una lista de
    -- posiciones si la primera esta a la iquierda, derecha, arrbia, etc
    -- de alguna de las posiciones de la lista.
    left _ [] = False
    left p@(c1, i1) ((c2,i2):rs) = (c1 == c2 && i1 < i2) || left p rs
    right _ [] = False
    right p@(c1, i1) ((c2,i2):rs) = (c1 == c2 && i1 > i2) || right p rs
    up _ [] = False
    up p@(c1, i1) ((c2,i2):rs) = (toInt c1 > toInt c2 && i2 == i1) || up p rs
    dawn _ [] = False
    dawn p@(c1, i1) ((c2,i2):rs) = (toInt c1 < toInt c2 && i2 == i1) || dawn p rs

    upLeft _ [] = False
    upLeft p@(c1, i1) ((c2,i2):rs) = (toInt c2 < toInt c1 && i1 < i2) || upLeft p rs
    upRight _ [] = False
    upRight p@(c1, i1) ((c2,i2):rs) = (toInt c2 < toInt c1 && i1 > i2) || upRight p rs
    dawnLeft _ [] = False
    dawnLeft p@(c1, i1) ((c2,i2):rs) = (toInt c1 < toInt c2 && i1 < i2) || dawnLeft p rs
    dawnRight _ [] = False
    dawnRight p@(c1, i1) ((c2,i2):rs) = (toInt c1 < toInt c2 && i1 > i2) || dawnRight p rs

--Posiblemente nos devuelva una pieza
getPieza :: Tablero -> Posicion -> Maybe Pieza
getPieza t (c,i) = (t ! c) ! i

--Nos dice si hay una pieza en la posicon dada
hayPieza :: Tablero -> Posicion -> Bool
hayPieza tab p =  Nothing /= getPieza tab p
