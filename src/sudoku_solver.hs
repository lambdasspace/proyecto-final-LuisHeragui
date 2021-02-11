import Data.Map as Map
import Data.Maybe

type Celda = String
type Valor = Int
type Tablero = Map.Map Celda [Valor]
data Tree a = Node a [Tree a]

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
main = putStrLn "hola"
