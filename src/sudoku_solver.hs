-- ----------------------------------------------------------------------------------- --
-- Progama que permite al usuario crear un juego de Sudoku clásico y observar si su    --
-- juego tiene una solución única o no.                                                --
-- ----------------------------------------------------------------------------------- --
module SudokuSolver where

import Data.Array
import Data.List
import Data.Maybe
import Data.Tree
import Control.Monad

-- Sinónimo para los valores del Sudoku.
type Valor = Char
-- Sinónimo para las celdas del tablero.
type Celda = (Char,Char)
-- Sinónimo para las unidades del tablero.
type Unidad = [Celda]
-- Sinónimo para el tablero del Sudoku.
type Tablero = Array Celda [Valor]
-- Sinónimo para el árbol de posibles tableros que lleven a una solución.
type TabTree = Tree (Maybe Tablero)

-- ----------------------------------------------------------------------------------- --
-- Preparación de los datos que necesitamos para establecer el tablero, así como para  --
-- resolver el Sudoku.                                                                 --
-- ----------------------------------------------------------------------------------- --
filas = "ABCDEFGHI"
columnas = "123456789"
digitos = "123456789"
area = (('A','1'),('I','9'))

-- | Función que recibe dos listas y regresa su producto cartesiano.
cartesiano :: [a] -> [b] -> [(a,b)]
cartesiano xs ys = (,) <$> xs <*> ys

-- | Definición de las celdas que tiene un tablero de Sudoku clásico.
celdas :: [Celda]
celdas = cartesiano filas columnas

-- | Definición de las unidades del tablero.
unidades :: [Unidad]
unidades = [cartesiano filas [c] | c <- columnas] ++
           [cartesiano [r] columnas | r <- filas] ++
           [cartesiano rs cs | rs <- ["ABC","DEF","GHI"], cs <- ["123","456","789"]]

{- | Definición de las unidades en las que se encuentra cada una de las celdas del
tablero.
-}
unisXcelda :: Array Celda [Unidad]
unisXcelda = array area [(c,[filter (/= c) u | u <- unidades, c `elem` u ]) | c <- celdas]

-- | Definición de todas las posibilidades de valores que tiene cada celda al inicio.
posibilidades :: Tablero
posibilidades = array area [(s,digitos) | s <- celdas]

-- | Definición de los amigos que tiene cada celda.
amigos :: Array Celda [Celda]
amigos = array area [(c,set (unisXcelda ! c)) | c <- celdas]
  where
    set = nub . concat

-- ----------------------------------------------------------------------------------- --
-- Obtención de los posibles valores para cada celda satisfaciendo las restricciones   --
-- que tiene un Sudoku, las cuales son:                                                --
-- (1) Si una celda sólo tiene un posible valor, entonces eliminamos ese valor de sus  --
--     amigos.                                                                         --
-- (2) Si una unidad sólo tiene una celda posible para colocar un valor, entonces ese  --
--     valor va en esa celda.                                                          --
-- ----------------------------------------------------------------------------------- --

{- | Función que recibe una lista @l@ de tuplas (Celda,Valor) y obtiene los posibles
valores que puede tener un tablero dada @l@ considerando las restricciones del Sudoku.
-}
aplicaRestricciones :: [(Celda,Valor)] -> Maybe Tablero
aplicaRestricciones cvs = foldM asigna posibilidades cvs

{- | Función que recibe un Tablero @t@, una tupla (Celda,Valor), y "asigna" a las celdas
de @t@ los posibles valores que pueden tener dada la tupla recibida. Esta función depende
completamente de `elimina` y básicamente se trata de eliminar todos los valores iniciales
que tiene una celda, menos el valor que le dio el usuario, además de checar las
restricciones.
-}
asigna :: Tablero -> (Celda,Valor) -> Maybe Tablero
asigna t (c,v) =
    if elem v digitos  -- Sólo nos importan los dígitos, no los demás carácteres.
    then do foldM elimina t (zip (repeat c) aEliminar)
    else return t
  where
    vals = t ! c
    aEliminar = delete v vals

{- | Función que recibe un Tablero @t@, una tupla (Celda,Valor), y elimina el Valor de
los posibles valores la Celda de @t@. Además, esta función se encarga de satisfacer
las restricciones checando cuando sólo queda un valor en la Celda, y evaluando cada
Valor eliminado en los amigos de la Celda para saber si hay alguna celda en la que
sólo pueda quedar ese valor.
-}
elimina :: Tablero -> (Celda,Valor) -> Maybe Tablero
elimina t (c,v)
    -- Si 'v' no se encuentra en los valores de c, entonces ya está eliminado.
    | notElem v vals = return t
    -- Si no, entonces eliminamos a 'v' de los valores de 'c'.
    | otherwise = do
        nuevoTab2 <- case valsNuevos of
            -- Esto no puede suceder, pues contradice a valsNuevos.
            []   -> Nothing
            -- Si sólo queda un valor en 'c', entonces lo eliminamos de sus amigos.
            [v'] -> do foldM elimina nuevoTab (zip amigosC (repeat v'))
            -- En otro caso, regresamos el nuevo tablero.
            _    -> return nuevoTab
        -- Por último checamos a los amigos de 'c' para saber si una unidad
        -- quedó reducida a sólo un valor 'v'.
        foldM (evalua v) nuevoTab2 (unisXcelda ! c)
  where
    vals = t ! c
    valsNuevos = delete v vals
    nuevoTab = t // [(c,valsNuevos)]
    amigosC = amigos ! c

{- | Función que recibe un Valor @v@, un Tablero @t@, una Unidad @u@, y se encarga de
satisfacer la segunda restricción checando si sólo queda una celda, dentro de una unidad,
en la que pueda pertenecer @v@.
-}
evalua :: Valor -> Tablero -> Unidad -> Maybe Tablero
evalua v t u = case unisValidas of
    []  -> Nothing
    [c] -> asigna t (c,v)  -- Segunda restricción.
    _   -> return t
  where
    -- Sólo nos fijamos en las unidades que puedan contener a 'v' en alguna celda.
    unisValidas = filter ((v `elem`) . (t !)) u

-- ----------------------------------------------------------------------------------- --
-- Aplicación de la heurística Minimum Remaining Values para elegir la celda cuyos     --
-- valores se probarán primero.                                                                --
-- ----------------------------------------------------------------------------------- --

{- | Función que recibe un árbol TabTree y busca la celda cuyos posibles valores sean los
mínimos para probar esos valores en el tablero contenido en la raíz del árbol. Por cada
valor probado se crea una nueva rama y se aplica la función recursivamente en cada nuevo
hijo.
-}
busca :: TabTree -> TabTree
busca (Node mt mts) =
    case mt of
        Nothing -> (Node mt [])  -- Rama sin solución.
        Just t  -> case [(length vs,(c,vs)) | (c,vs) <- assocs $ t,length vs /= 1] of
            [] -> Node (Just t) []  -- Rama con una solución.
            ls -> Node (Just t) $
                fmap busca (actualizaHijos mts $ map (\v -> asigna t (c,v)) vals)
              where
                (_,(c,vals)) = minimum ls

{- | Función que recibe una lista de árboles TabTree @ts@, una lista de (Maybe Tablero)
@mts@, y se encarga de agregar cada tablero en @mts@ a @ts@. Ésto para actualizar los
hijos de cada nodo y poder buscar sobre ellos.
-}
actualizaHijos :: [TabTree] -> [Maybe Tablero] -> [TabTree]
actualizaHijos ts [] = ts
actualizaHijos ts (mt:mts) = actualizaHijos (ts ++ [Node mt []]) mts

-- ----------------------------------------------------------------------------------- --
-- Muestra de las soluciones obtenidas después de aplicar las dos estrategias al       --
-- Sudoku propuesto.                                                                          --
-- ----------------------------------------------------------------------------------- --

{- | Función que recibe un árbol TabTree y regresa una lista que sólo contiene los
elementos del árbol que son soluciones al Sudoku.
-}
obtenSoluciones :: TabTree -> [Tablero]
obtenSoluciones (Node mt []) = case mt of
    Nothing -> []
    Just t  -> [t]
obtenSoluciones (Node mt mts) = concatMap obtenSoluciones mts

{- | Función que recibe una lista de tableros (que deben ser soluciones al Sudoku) y
muestra en pantalla una solución sólo si las posibles soluciones son menores a 1000.
-}
muestraSoluciones :: [Tablero] -> IO ()
muestraSoluciones [] = putStrLn "No hay solución.\n"
muestraSoluciones ts =
    if numSols == 1
    then do
        putStrLn "Su sudoku tiene 1 solución."
        putStrLn "La solución es:"
        imprimeTablero $ head sols
    else if numSols < 1000
        then do
            putStrLn $ "Su sudoku tiene " ++ show numSols ++ " soluciones."
            putStrLn "Una posible solución es:"
            imprimeTablero $ head sols
        else do
            putStrLn "Su sudoku es horrible y tiene más de 1000 soluciones."
            putStrLn "Una posible solución es:"
            imprimeTablero $ head sols
  where
    sols = take 1000 ts
    numSols = length sols

-- ----------------------------------------------------------------------------------- --
-- Impresión de un tablero de Sudoku en la pantalla.                                   --
-- ----------------------------------------------------------------------------------- --

-- | Definición del tablero de ejemplo para que el usuario conozca las celdas.
ejemplo :: Tablero
ejemplo = array area [(s,(fst s) : (snd s) : []) | s <- celdas]

-- | Función que recibe un tablero y lo imprime en la pantalla.
imprimeTablero :: Tablero -> IO ()
imprimeTablero = putStrLn . tableroToString

{- | Función que recibe un tablero y realiza un análisis para convertirlo en una cadena
y que pueda ser impreso en la pantalla.
-}
tableroToString :: Tablero -> String
tableroToString g = "\n" ++ unlines l5
  where
    l0 = elems g  -- ["12347","2689",...]
    l1 = map (\s -> " " ++ s ++ " ") l0  -- [" 1 "," 2 ",...]
    l2 = map concat $ subdivide 3 l1  -- [" 1  2  3 "," 4  5  6 ",...]
    l3 = subdivide 3 l2  -- [[" 1  2  3 "," 4  5  6 "," 7  8  9 "],...]
    l4 = map (concat . intersperse "|") l3  -- [" 1  2  3 | 4  5  6 | 7  8  9 ",...]
    l5 = concat $ intersperse [linea] $ subdivide 3 l4  -- Añade líneas divisorias.
    long    = div (maximum $ map length l4) 3
    guiones = replicate long '-'
    linea   = guiones ++ "+" ++ guiones ++ "+" ++ guiones

{- | Función que recibe un entero @n@, una lista de cadenas @l@, y agrupa a los elementos
de @l@ en listas de tamaño @n@.
-}
subdivide :: Int -> [String] -> [[String]]
subdivide n [] = []
subdivide n xs = ys : subdivide n zs
  where
    (ys,zs) = splitAt n xs

-- ----------------------------------------------------------------------------------- --
-- Obtención y validación de los datos ingresados por el usuario.                      --
-- ----------------------------------------------------------------------------------- --

-- Declaración de las celdas que son válidas para que ingrese el usuario.
celdasValidas = [f : c : [] | f <- filas,c <- columnas]

{- | Función que recibe una lista de cadenas @l@, una lista de tuplas (Celda,Valor), un
tablero, y va solicitando al usuario un valor para cada elemento de @l@, que deben ser
casillas, guardando el valor en la lista de tuplas, y mostrando un tablero actualizado
con el valor que ingresó el usuario en la celda respectiva.
-}
obtenValores :: [String] -> [(Celda,Valor)] -> Tablero -> IO [(Celda,Valor)]
obtenValores [] cv muestra = return cv
obtenValores (c:cs) cv muestra = do
    putStr $ "\nIngrese un valor entre 1 y 9 para la celda " ++ c ++ ": "
    x <- getChar
    if elem x digitos
        then do
            let celda = (head c,last c)
                muestra' = muestra // [(celda,[x,' '])]
            putStr "\n"
            imprimeTablero muestra'
            obtenValores cs (cv ++ [(celda,x)]) muestra'
        else do
            putStrLn "\nPor favor ingrese un valor correcto."
            obtenValores (c:cs) cv muestra

{- | Función que solicita al usuario las celdas en las que va a ingresar valores,
los valida, y regresa una lista de tuplas (Celdas,Valor) con todas las celdas del
tablero y los respectivos valores que ingresó el usuario.
-}
solicitaCeldas :: IO [(Celda,Valor)]
solicitaCeldas = do
    putStrLn "\n\n¿En qué celdas desea colocar valores?"
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
            putStr "\nPor favor siga el ejemplo e ingrese celdas válidas."
            solicitaCeldas

{- | Función que recibe una lista de celdas, dos listas de tuplas (Celda,Valor), y va
actualizando cada una de las celdas del tablero con los valores que ingresó el usuario,
o con un '.' si el usuario no ingresó un valor para la celda.
-}
actualiza :: [Celda] -> [(Celda,Valor)] -> [(Celda,Valor)] -> [(Celda,Valor)]
actualiza [] _ _ = []
actualiza (c:cs) [] xs = [(c,'.')] ++ actualiza cs xs []
actualiza (c:cs) (cv:cvs) xs
    | c == fst cv = [cv] ++ actualiza cs (xs ++ cvs) []
    | otherwise = actualiza (c:cs) cvs (xs ++ [cv])

-- ----------------------------------------------------------------------------------- --
-- Interacción con el usuario.                                 --
-- ----------------------------------------------------------------------------------- --

{- | Función que muestra un menú sencillo con dos opciones para que el usuario pueda
interactuar con el programa.
-}
muestraMenu :: IO Char
muestraMenu = do
    putStrLn "¿Qué desea hacer?"
    putStrLn "[1] Resolver un Sudoku."
    putStrLn "[2] Salir."
    putStr "Opción: "
    opcion <- getChar
    return opcion

{- | Función que recibe un carácter y, dependiendo de su valor, resuelve un Sudoku o
termina el programa.
-}
procesa :: Char -> IO ()
procesa '1' = do
    v <- solicitaCeldas
    case aplicaRestricciones v of
        Nothing -> putStrLn "No hay solución.\n"
        Just s  -> muestraSoluciones $ obtenSoluciones $ busca (Node (Just s) [])
procesa '2' = putStrLn "\n\nSaliendo...\n"
procesa _ = do
    putStr "\n\nOpción no válida.\n"
    main

-- | Función principal del programa.
main :: IO ()
main = do
    putStr "\ESC[2J"
    putStrLn "\n---+---SUDOKU SOLVER---+---\n"
    m <- muestraMenu
    procesa m
