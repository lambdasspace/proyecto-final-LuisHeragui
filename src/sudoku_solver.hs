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
digitos = "123456789"
area = (('A','1'),('I','9'))

cartesiano :: [a] -> [b] -> [(a, b)]
cartesiano xs ys = (,) <$> xs <*> ys

celdas :: [Celda]
celdas = cartesiano filas columnas

unidades :: [Unidad]
unidades = [cartesiano filas [c] | c <- columnas] ++ [cartesiano [r] columnas | r <- filas] ++
           [cartesiano rs cs | rs <- ["ABC","DEF","GHI"], cs <- ["123","456","789"]]

unisXcelda :: Array Celda [Unidad]
unisXcelda = array area [(s, [filter (/= s) u | u <- unidades, s `elem` u ]) | s <- celdas]

posibilidades :: Tablero
posibilidades = array area [(s, digitos) | s <- celdas]

amigos :: Array Celda [Celda]
amigos = array area [(c, set (unisXcelda!c)) | c <- celdas]
  where
    set = nub . concat

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

obtenValores :: IO ()
obtenValores = do putStr "hola"

muestraMenu :: IO Char
muestraMenu = do
    putStrLn "¿Qué desea hacer?"
    putStrLn "[1] Resolver un Sudoku."
    putStrLn "[2] Salir."
    putStr "Opción: "
    opcion <- getChar
    return opcion

procesa :: Char -> IO ()
procesa '1' = obtenValores
procesa '2' = putStrLn "\n\nSaliendo...\n"
procesa _ = putStr "\n\nOpción no válida.\n\n"

main :: IO ()
main = do
    putStrLn "---SUDOKU SOLVER---\n"
    m <- muestraMenu
    procesa m
