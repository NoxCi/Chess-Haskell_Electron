module Modulos.Pieza where

import Data.Char (isDigit, toUpper)

data Tipo = Torre | Caballo | Alfil | Rey | Reina | Peon deriving Eq
instance Show Tipo where
  show Torre = "T"
  show Caballo = "C"
  show Alfil = "A"
  show Rey = "R"
  show Reina = "Q"
  show Peon = "P"

data Color = B | N deriving (Show,Eq)

type Pieza = (Tipo, Color)
type Posicion = (Char, Integer)

--Nos devuelve todas los posibles movimientos de una pieza dada su posicion en el tablero
posiblesMovimientos :: Pieza -> Posicion -> [Posicion]
posiblesMovimientos pieza p@(c,i) = let
  j = toInt c
  s = i + j
  d = j - i
  in case pieza of
    (Torre,_) -> [(toChar a,b) | a <- [1..8], b <- [1..8], b == i || a == j]

    (Caballo,_) -> filter posicionValida [(toChar (j+2), i+1), (toChar (j+2), i-1), --arriba
                                         (toChar (j-2), i+1), (toChar (j-2), i-1), --abajo
                                         (toChar (j+1), i+2), (toChar (j-1), i+2), -- derecha
                                         (toChar (j+1), i-2), (toChar (j-1), i-2)] --izquierda

    (Alfil,_) -> [(toChar a,b) | a <- [1..8], b <- [1..8], a+b == s || a == b+d]

    (Rey,_) -> filter posicionValida [(toChar (j+1), i), (toChar (j+1), i+1),
                                     (toChar (j), i+1), (toChar (j-1), i+1),
                                     (toChar (j-1), i), (toChar (j-1), i-1),
                                     (toChar (j), i-1), (toChar (j+1), i-1)]

    (Reina,_) -> [(toChar a,b) | a <- [1..8], b <- [1..8], a+b == s || a == b+d || b == i || a == j]

    (Peon,B) -> if j == 2
                then filter posicionValida [(toChar (j+1), i), (toChar (j+1), i+1), (toChar (j+1), i-1), (toChar (j+2), i)]
                else filter posicionValida[(toChar (j+1), i), (toChar (j+1), i+1), (toChar (j+1), i-1)]

    (Peon,N) -> if j == 7
                then filter posicionValida [(toChar (j-1), i), (toChar (j-1), i+1), (toChar (j-1), i-1), (toChar (j-2), i)]
                else filter posicionValida[(toChar (j-1), i), (toChar (j-1), i+1), (toChar (j-1), i-1)]


--Nos dice dado una posicion es valida
posicionValida :: Posicion -> Bool
posicionValida (c,j) = ((toChar . toInt) c /= ' ') && ((toInt . toChar) j /= -1)

--Intenta crear una posicion a partir de una String, valida o no,
--devuelve Nothing si no pudo.
makePosicion :: String -> Maybe Posicion
makePosicion st
  | length st < 3 = Nothing
  | otherwise = let
    x = st !! 0
    y = st !! 2
    in if isDigit x && isDigit y
      then case read [y] of
        1 -> Just ('A', read [x])
        2 -> Just ('B', read [x])
        3 -> Just ('C', read [x])
        4 -> Just ('D', read [x])
        5 -> Just ('E', read [x])
        6 -> Just ('F', read [x])
        7 -> Just ('G', read [x])
        8 -> Just ('H', read [x])
      else Nothing

--Crea un nuevo formato de una nueva posicion para que pueda ser leido por el
--usuario repecto a las nuevas coordenadas.
newFormat :: Posicion -> Posicion
newFormat (c, n) = (toCharNF n,toInt c)

--Devuelve el color de una pieza
color :: Pieza -> Color
color (_, c) = c

--Devuelve el tipo de una pieza
tipo :: Pieza -> Tipo
tipo (t,_) = t

toInt :: Char -> Integer
toInt c = case c of
  'A' -> 8
  'B' -> 7
  'C' -> 6
  'D' -> 5
  'E' -> 4
  'F' -> 3
  'G' -> 2
  'H' -> 1
  otherwise -> -1

toChar :: Integer -> Char
toChar i = case i of
   8 -> 'A'
   7 -> 'B'
   6 -> 'C'
   5 -> 'D'
   4 -> 'E'
   3 -> 'F'
   2 -> 'G'
   1 -> 'H'
   otherwise -> ' '

toCharNF :: Integer -> Char
toCharNF i = case i of
   8 -> 'H'
   7 -> 'G'
   6 -> 'F'
   5 -> 'E'
   4 -> 'D'
   3 -> 'C'
   2 -> 'B'
   1 -> 'A'
   otherwise -> ' '
