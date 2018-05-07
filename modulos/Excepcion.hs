module Modulos.Excepcion where

data Excepcion = MovInvalido
               | NoHayPiezaInicial
               | PosicionInicialInexistente
               | PosicionFinalInexistente
               | PosicionInexistente
               | PiezaContraria deriving Show
