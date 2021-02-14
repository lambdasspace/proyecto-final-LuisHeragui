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

muestraSolucion :: IO ()
muestraSolucion = putStrLn "hola"

muestraNumSoluciones :: IO ()
muestraNumSoluciones = putStrLn "hola"
-----------------------------Impresión
ejemplo :: Tablero
ejemplo = array area [(s, (fst s) : (snd s) : []) | s <- celdas]

imprimeTablero :: Tablero -> IO ()
imprimeTablero = putStrLn . tableroToString

tableroToString :: Tablero -> String
tableroToString g = "\n" ++ unlines l5
  where
    l0 = elems g -- ["12347","2689",...]
    l1 = map (\s -> " " ++ s ++ " ") l0 -- [" 1 "," 2 ",...]
    l2 = map concat $ subdivide 3 l1 -- [" 1  2  3 "," 4  5  6 ",...]
    l3 = subdivide 3 l2 -- [[" 1  2  3 "," 4  5  6 "," 7  8  9 "],...]
    l4 = map (concat . intersperse "|") l3 -- [" 1  2  3 | 4  5  6 | 7  8  9 ",...]
    l5 = concat $ intersperse [linea] $ subdivide 3 l4 -- añade líneas divisorias
    long    = div (maximum $ map length l4) 3
    guiones = replicate long '-'
    linea   = guiones ++ "+" ++ guiones ++ "+" ++ guiones

subdivide :: Int -> [String] -> [[String]]
subdivide n [] = []
subdivide n xs = ys : subdivide n zs
  where
    (ys,zs) = splitAt n xs

-----------------------------Validación y preparación
celdasValidas = [f : c : [] | f <- filas, c <- columnas]

obtenValores :: [String] -> [(Celda, Valor)] -> Tablero -> IO [(Celda, Valor)]
obtenValores [] cv muestra = return cv
obtenValores (c:cs) cv muestra = do
    putStr $ "\nIngresa un valor entre 1 y 9 para " ++ c ++ ": "
    x <- getChar
    if elem x digitos
        then do
            let celda = (head c, last c)
                muestra' = muestra // [(celda, [x, ' '])]
            putStr "\n"
            imprimeTablero muestra'
            obtenValores cs (cv ++ [(celda, x)]) muestra'
        else do
            putStrLn "\nPor favor ingrese un valor correcto."
            obtenValores (c:cs) cv muestra

solicitaCeldas :: IO [(Celda, Valor)]
solicitaCeldas = do
    putStrLn "\n¿En qué celdas desea colocar valores?"
    putStrLn "Ejemplo: A1 C4 D9 I2"
    imprimeTablero ejemplo
    putStr "Celdas: "
    cs <- getLine
    let cels = words cs
    if all (`elem` celdasValidas) cels == True
        then do
            v <- obtenValores cels [] ejemplo
            return (actualiza celdas v [])
        else do
            putStr "\nPor favor siga el ejemplo e ingrese celdas válidas.\n"
            solicitaCeldas

actualiza :: [Celda] -> [(Celda, Valor)] -> [(Celda, Valor)] -> [(Celda, Valor)]
actualiza [] _ _ = []
actualiza (c:cs) [] xs = [(c, '.')] ++ actualiza cs xs []
actualiza (c:cs) (cv:cvs) xs
    | c == fst cv = [cv] ++ actualiza cs (xs ++ cvs) []
    | otherwise = actualiza (c:cs) cvs (xs ++ [cv])

-----------------------------Menú
muestraMenu :: IO Char
muestraMenu = do
    putStrLn "¿Qué desea hacer?"
    putStrLn "[1] Resolver un Sudoku."
    putStrLn "[2] Salir."
    putStr "Opción: "
    opcion <- getChar
    return opcion

procesa :: Char -> IO ()
procesa '1' = do
    v <- solicitaCeldas
    putStr "Bien"
procesa '2' = putStrLn "\n\nSaliendo...\n"
procesa _ = putStr "\n\nOpción no válida.\n\n"

main :: IO ()
main = do
    putStrLn "\n---SUDOKU SOLVER---\n"
    m <- muestraMenu
    procesa m
