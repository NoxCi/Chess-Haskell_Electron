module Main where

import Modulos.Tablero
import Modulos.Pieza
import Modulos.Pruebas
import System.Process
import Data.Maybe

main = do
  let tablero = creaTableroInicial
  callCommand "cls"
  putStr$ "Jugador 1: Blancas | Jugador 2: Negras\n\n" ++
            "T: Torre    N: Negras\n" ++
            "C: Caballo  B: Blancas\n" ++
            "A: Alfil\n" ++
            "R: Rey\n" ++
            "Q: Reina\n" ++
            "P: Peon\n\n" ++
            "Para introducir una posicion solo basta con introducir la letra seguido del número\n" ++
            "Ej. a2\n" ++
            "Ej. A2\n" ++
            "Enter para continuar: "
  _ <- getLine
  loop tablero [] Nothing 0 ""


loop t@tablero ls mPI i msg = do
  if even i --Jugador 1

    then if null ls --seleccion de posicion inicial
      then do
        callCommand "cls"
        putStr $ dibujaTablero t [] ++ "\n" ++
               "     Turno Jugador 1               " ++ msg ++ "\n" ++
               "Posición inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero [] Nothing i "Posición invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == N
              then loop tablero [] Nothing i "Posición invalida"
              else loop tablero (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand "cls"
        putStr $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 1               " ++ msg ++ "\n" ++
               "Posición final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero ls mPI i "Posición invalida"
            else do
              let pI = fromJust mPI
              case muevePieza t pI pF  of
                (_, Nothing) -> loop tablero ls mPI i "Posición invalida"
                (_, Just t') -> loop t' [] Nothing (i+1) (show pI)

    --Jugador 2
    else if null ls --seleccion de posicion inicial
      then do
        callCommand "cls"
        putStr $ dibujaTablero t [] ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posición inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero [] Nothing i "Posición invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == B
              then loop tablero [] Nothing i "Posición invalida"
              else loop tablero (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand "cls"
        putStr $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posición final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero ls mPI i "Posición invalida"
            else do
              let pI = fromJust mPI
              case muevePieza t pI pF  of
                (_, Nothing) -> loop tablero ls mPI i "Posición invalida"
                (_, Just t') -> loop t' [] Nothing (i+1) (show pI)
