module Main where

import Modulos.Tablero(Tablero,
                       creaTableroInicial,
                       dibujaTablero,
                       codificaTablero,
                       muevePieza,
                       dropInalcanzables,
                       getPieza)
import Modulos.Pieza(Color(..),
                     Pieza,
                     Posicion,
                     makePosicion,
                     color,
                     posiblesMovimientos,
                     posicionValida,
                     newFormat)
import Modulos.Pruebas
import System.Process (callCommand)
import System.Info (os)
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay,
                           forkOS,
                           myThreadId,
                           killThread)
import Control.Exception(SomeException(..),
                         catch,
                         throwIO)
import System.IO.Error(IOError,
                       userError)

fileP_O = "shared_files/Ohs_Ijs.txt"
fileP_I = "shared_files/Ihs_Ojs.txt"

main :: IO ()
main = do
  let tablero = creaTableroInicial
  output fileP_O $ "1\n" ++ "Turno jugador 1 --- " ++ "" ++ "\n" ++ codificaTablero tablero []
  idT_1 <- forkOS $ callCommand "npm start"
  catch (app tablero [] Nothing 0 "" '.') handler

app :: Tablero -> [Posicion] -> Maybe Posicion -> Integer -> String -> Char -> IO ()
app t@tablero ls mPI i msg f = do
  if even i --Jugador 1

    then if null ls --seleccion de posicion inicial
      then do
        output fileP_O $ "1\n" ++ "Turno jugador 1 --- " ++ msg ++ "\n" ++ codificaTablero tablero ls
        (p,f') <- input fileP_I f
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then app tablero [] Nothing i "Posicion invalida(1)" f'
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == N
              then app tablero [] Nothing i "Posicion invalida(2)" f'
              else app tablero (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i "" f'

      else do  --seleccion de posicion final
        output fileP_O $ "1\n" ++ "Turno jugador 1 --- " ++ msg ++ "\n" ++ codificaTablero tablero ls
        (p,f') <- input fileP_I f
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then app tablero ls mPI i "Posicion invalida(3)" f'
            else do
              let pI = fromJust mPI
              if pF == pI
                then app tablero [] Nothing i "" f'
                else if not (elem pF ls)
                  then app tablero ls mPI i "Posicion invalida(4)" f'
                  else app (muevePieza t pI pF) [] Nothing (i+1) ("Ultimo movimiento: " ++ show (newFormat pI) ++" > "++show (newFormat pF)) f'

    --Jugador 2
    else if null ls --seleccion de posicion inicial
      then do
        output fileP_O $ "2\n" ++ "Turno jugador 2 --- " ++ msg ++ "\n" ++ codificaTablero tablero ls
        (p,f') <- input fileP_I f
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then app tablero [] Nothing i "Posicion invalida(5)" f'
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == B
              then app tablero [] Nothing i "Posicion invalida(6)" f'
              else app tablero (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i "" f'

      else do  --seleccion de posicion final
        output fileP_O $ "2\n" ++ "Turno jugador 2 --- " ++ msg ++ "\n" ++ codificaTablero tablero ls
        (p,f') <- input fileP_I f
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then app tablero ls mPI i "Posicion invalida(7)" f'
            else do
              let pI = fromJust mPI
              if pF == pI
                then app tablero [] Nothing i "" f'
                else if not (elem pF ls)
                  then app tablero ls mPI i "Posicion invalida(8)" f'
                  else app (muevePieza t pI pF) [] Nothing (i+1) ("Ultimo movimiento: " ++ show (newFormat pI) ++" > "++show (newFormat pF)) f'


input :: FilePath -> Char -> IO (String, Char)
input filePath flag= do
  threadDelay 100000
  inP <- readFile filePath
  if length inP == 0
    then input filePath flag
    else if inP !! 0 /= flag
      then return (tail inP, inP !! 0)
      else input filePath flag

output :: FilePath -> String -> IO ()
output filePath text = do
  writeFile filePath text

handler :: SomeException -> IO ()
handler _ = do
  id_T <- myThreadId
  putStrLn "."
  output fileP_O ""
  killThread id_T

------------------------
--Condigo de prototipo--
------------------------
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
               "Posicion inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero cmd [] Nothing i "Posicion invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == N
              then loop tablero cmd [] Nothing i "Posicion invalida"
              else loop tablero cmd (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand cmd
        putStrLn $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 1               " ++ msg ++ "\n" ++
               "Posicion final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero cmd ls mPI i "Posicion invalida"
            else do
              let pI = fromJust mPI
              if not (elem pF ls)
                then loop tablero cmd ls mPI i "Posicion invalida"
                else loop (muevePieza t pI pF) cmd [] Nothing (i+1) ""

    --Jugador 2
    else if null ls --seleccion de posicion inicial
      then do
        callCommand cmd
        putStrLn $ dibujaTablero t [] ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posicion inicial: "
        p <- getLine
        let mPI' = makePosicion p
            pI = fromJust mPI'
        if mPI' == Nothing || not (posicionValida pI)
          then loop tablero cmd [] Nothing i "Posicion invalida"
          else do
            let mPieza = getPieza t pI
                pieza = fromJust mPieza
            if mPieza == Nothing || color pieza == B
              then loop tablero cmd [] Nothing i "Posicion invalida"
              else loop tablero cmd (dropInalcanzables t pieza pI (posiblesMovimientos pieza pI)) mPI' i ""

      else do  --seleccion de posicion final
        callCommand cmd
        putStrLn $ dibujaTablero t ls ++ "\n" ++
               "     Turno Jugador 2               " ++ msg ++ "\n" ++
               "Posicion final: "
        p <- getLine
        let
          mPF = makePosicion p
          pF = fromJust mPF
          in if mPF == Nothing
            then loop tablero cmd ls mPI i "Posicion invalida"
            else do
              let pI = fromJust mPI
              if not (elem pF ls)
                then loop tablero cmd ls mPI i "Posicion invalida"
                else loop (muevePieza t pI pF) cmd [] Nothing (i+1) ""
