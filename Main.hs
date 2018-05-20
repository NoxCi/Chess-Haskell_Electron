module Main where

import Modulos.Tablero(creaTableroInicial,
                       dibujaTablero,
                       codificaTablero,
                       muevePieza,
                       dropInalcanzables,
                       getPieza)
import Modulos.Pieza(Color(..),
                     makePosicion,
                     color,
                     posiblesMovimientos,
                     posicionValida)
import Modulos.Pruebas
import System.Process (callCommand)
import System.Info (os)
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay,
                           forkOS)

fileP_O = "shared_files/Ohs_Ijs.txt"
fileP_I = "shared_files/Ihs_Ojs.txt"

main :: IO ()
main = do
  output fileP_O $ "1\n" ++ codificaTablero creaTableroInicial []
  idT_1 <- forkOS $ callCommand "npm start"
  --app
  return ()

app :: IO ()
app = do
  inP <- input fileP_I
  let outP =  inP --aqui debe procesarse la entrada
  output fileP_O outP
  app

input :: FilePath -> IO String
input filePath = do
  threadDelay 100000
  readFile filePath

output :: FilePath -> String -> IO ()
output filePath text = do
  writeFile filePath text

--Condigo de prototipo--
main' :: IO ()
main' = do
  let tablero = creaTableroInicial
      cmd = if os == "mingw32"
        then "cls"
        else "clear"
  callCommand cmd
  putStrLn$ "Jugador 1: Blancas | Jugador 2: Negras\n\n" ++
            "T: Torre    N: Negras\n" ++
            "C: Caballo  B: Blancas\n" ++
            "A: Alfil\n" ++
            "R: Rey\n" ++
            "Q: Reina\n" ++
            "P: Peon\n\n" ++
            "Para introducir una posicion solo basta con introducir la letra seguido del número\n" ++
            "Para salir introducir precionar Ctlr+C\n" ++
            "Ej. a2\n" ++
            "Ej. A2\n" ++
            "Enter para continuar: "
  _ <- getLine
  loop tablero cmd [] Nothing 0 ""


loop t@tablero cmd ls mPI i msg = do
  if even i --Jugador 1

    then if null ls --seleccion de posicion inicial
      then do
        callCommand cmd
        putStrLn $ dibujaTablero t [] ++ "\n" ++
               "     Turno Jugador 1               " ++ msg ++ "\n" ++
               "Posición inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero cmd [] Nothing i "Posición invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == N
              then loop tablero cmd [] Nothing i "Posición invalida"
              else loop tablero cmd (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand cmd
        putStrLn $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 1               " ++ msg ++ "\n" ++
               "Posición final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero cmd ls mPI i "Posición invalida"
            else do
              let pI = fromJust mPI
              if not (elem pF ls)
                then loop tablero cmd ls mPI i "Posición invalida"
                else loop (muevePieza t pI pF) cmd [] Nothing (i+1) ""

    --Jugador 2
    else if null ls --seleccion de posicion inicial
      then do
        callCommand cmd
        putStrLn $ dibujaTablero t [] ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posición inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero cmd [] Nothing i "Posición invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == B
              then loop tablero cmd [] Nothing i "Posición invalida"
              else loop tablero cmd (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand cmd
        putStrLn $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posición final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero cmd ls mPI i "Posición invalida"
            else do
              let pI = fromJust mPI
              if not (elem pF ls)
                then loop tablero cmd ls mPI i "Posición invalida"
                else loop (muevePieza t pI pF) cmd [] Nothing (i+1) ""
