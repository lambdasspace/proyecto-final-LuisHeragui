import Data.Array
import Data.List
import Data.Maybe
import Control.Monad

type Valor = Char
type Celda = (Char, Char)
type Unidad = [Celda]

type Tablero = Array Celda [Valor]

data Tree a = Node a [Tree a]

filas = "ABCDEFGHI"
columnas = "123456789"

cartesiano :: [a] -> [b] -> [(a, b)]
cartesiano xs ys = (,) <$> xs <*> ys

celdas :: [Celda]
celdas = cartesiano filas columnas

asigna :: Tablero -> (Celda, Valor) -> Maybe Tablero
asigna t (c, v) = Nothing

elimina :: Tablero -> (Celda, Valor) -> Maybe Tablero
elimina t (c, v) = Nothing

busca :: Tablero -> Maybe Tablero
busca t = Nothing

obtenSolucion :: Tree Tablero -> Tablero
obtenSolucion (Node a []) = a

obtenNumSoluciones :: Tree Tablero -> Int
obtenNumSoluciones (Node a []) = 0

agregaValor :: IO ()
agregaValor = putStrLn "hola"

muestraSolucion :: IO ()
muestraSolucion = putStrLn "hola"

muestraNumSoluciones :: IO ()
muestraNumSoluciones = putStrLn "hola"

procesa :: Char -> IO ()
procesa c = putStrLn "hola"

muestraMenu :: IO Char
muestraMenu = return 'c'

main :: IO ()
main = do
    putStrLn "SUDOKU SOLVER \n"
