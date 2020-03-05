-----------------------------------------------------------------------
-- TALLER DE ALGEBRA I
-- Verano 2020

-- NUMERO DE GRUPO: 2 

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1:
-- INTEGRANTE 2:
-- INTEGRANTE 3:
-----------------------------------------------------------------------
module SolucionTP
( maximo )
where

type Conjunto a = [a]
type Fila = [Integer]
type Tablero = [Fila]
type Posicion = (Integer,Integer)
type Camino = [Posicion]

sopa1 :: Tablero
sopa1 = [[13,12,6,4],[1,1,32,25],[9,2,16,8],[7,3,5,4],[1,2,8,2]]

sopa2 :: Tablero
sopa2 = [[64,5,16,8,9],[32,16,8,4,28],[2,1,4,6,3],[8,4,2,1,10]]

sopa3 :: Tablero
sopa3 = [[10,5,7,9],[32,16,22,28],[8,4,11,34],[12,6,10,17]]

sopa4 :: Tablero
sopa4 = [[1,1,2,1], [1,8,4,1], [10,1,1,1], [5,16,1,1]]

camino1 :: Camino 
camino1 = [(3,3),(3,4),(4,4)]

camino2 :: Camino 
camino2 = [(1,2),(2,2),(2,3),(3,3),(4,3)]

camino3 :: Camino 
camino3 = [(1,3),(2,3),(3,3),(3,4)]

camino4 :: Camino
camino4 = [(4,1),(4,2),(4,3)]

-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 

-----------------------------------------------------------------------
-- CONJUNTOS
-----------------------------------------------------------------------
-- Operaciones basicas de conjuntos
vacio :: Conjunto a
vacio = []

pertenece :: Eq a => a -> Conjunto a -> Bool
pertenece _ [] = False
pertenece x (y:cy)
  | x == y = True
  | otherwise = pertenece x cy

agregar :: Eq a => a -> Conjunto a -> Conjunto a
agregar x cy 
  | pertenece x cy = cy
  | otherwise = x:cy

-- Operaciones complejas sobre conjuntos
union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union [] cy = cy
union (x:cx) cy = agregar x (union cx cy)

interseccion :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccion [] _ = []
interseccion (x:cx) cy
  | pertenece x cy = agregar x ( interseccion cx cy)
  | otherwise = interseccion cx cy

incluido :: Eq a => Conjunto a -> Conjunto a -> Bool
incluido [] _ = True
incluido (x:cx) cy
  | pertenece x cy = incluido cx cy
  | otherwise = False

iguales :: Eq a => Conjunto a -> Conjunto a -> Bool
iguales cx cy = (incluido cx cy) && (incluido cy cx)

-- Permutaciones
-- Dado un n obtener un conjunto con todas las permutaciones de [1, 2, .., n].
-- La cantidad de elementos [Integer] retornados es n!
permutaciones :: Integer -> Conjunto [Integer]
permutaciones 1 = agregar [1] vacio
permutaciones n = insertarEnTodaListaEnTodaPos n (permutaciones (n-1))

-- Inserta n en toda posicion de la lista xs hasta la posicion k
insertarEnTodaPosHasta :: Integer -> [Integer] -> Integer -> Conjunto [Integer]
insertarEnTodaPosHasta n xs 1 = agregar (n:xs) vacio
insertarEnTodaPosHasta n xs k = agregar (insertarEnPos n xs k) (insertarEnTodaPosHasta n xs (k-1)) 

-- Inserta n en la lista dada (x:xs) en la posicion k
insertarEnPos :: Integer -> [Integer] -> Integer -> [Integer]
insertarEnPos n xs 1 = n:xs
insertarEnPos n (x:xs) k = x:(insertarEnPos n xs (k-1))

-- Inserta el valor en en todas las listas del conjunto xs en todas las posiciones
-- Ej. [[1 2] [2 1]] -> [[n 1 2] [1 n 2] [1 2 n] [n 2 1] [2 n 1] [2 1 n]]
-- Si la cantidad de elementos de xs es k, entonces la cantidad de elementos retornados
-- es k*(k+1).
insertarEnTodaListaEnTodaPos :: Integer -> Conjunto [Integer] -> Conjunto [Integer]
insertarEnTodaListaEnTodaPos n [] = vacio 
insertarEnTodaListaEnTodaPos n (x:cxs) = union (insertarEnTodaPosicion n x) (insertarEnTodaListaEnTodaPos n cxs)

insertarEnTodaPosicion :: Integer -> [Integer] -> Conjunto [Integer]
insertarEnTodaPosicion n xs = insertarEnTodaPosHasta n xs (toInteger(length xs) + 1) 

-----------------------------------------------------------------------
-- SOLUCIONES
-----------------------------------------------------------------------
-- Ejercicio 1
maximoElemento :: Integer -> Integer -> Integer
maximoElemento x y
  | x >= y = x
  | otherwise = y

maximoFila :: Fila -> Integer
maximoFila [] = 0
maximoFila (x:xs) = maximoElemento x (maximoFila xs)

maximo :: Tablero -> Integer
maximo [] = 0
maximo (f:fs) = maximoElemento (maximoFila f) (maximo fs)

-- Ejercicio 2

-- contarRepeticiones :: Fila -> Integer -> (Integer, Integer)
-- contarRepeticiones [y] c = (y, c)
-- contarRepeticiones (y:x:xs) c
--   | y == x = contarRepeticiones (x:xs) (c+1)
--   | otherwise = (y, c)

masRepetidoElemento :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
masRepetidoElemento (x, cx) (y, cy)
  | cx >= cy = (x, cx)
  | otherwise = (y, cy)

-- masRepetidoFila :: Fila -> (Integer, Integer)
-- masRepetidoFila [] = (0, 0) 
-- masRepetidoFila (x:xs) = masRepetidoElemento (contarRepeticiones (x:xs) 0) (masRepetidoFila xs)

-- masRepetidoVeces :: Tablero -> (Integer, Integer)
-- masRepetidoVeces [] = (0, 0)
-- masRepetidoVeces (f:fs) = masRepetidoElemento (masRepetidoFila (ordenar f)) (masRepetidoVeces fs)

-- masRepetido :: Tablero -> Integer
-- masRepetido t = fst (masRepetidoVeces t)


-- ordeno la fila 
-- busco max -> (e,e)
--

masRepetidoFila :: (Integer, Integer) -> Fila -> Integer -> (Integer, Integer)
masRepetidoFila m [x] c = masRepetidoElemento m (x, c)
masRepetidoFila m (y:x:xs) c
  | x == y = masRepetidoElemento m (masRepetidoFila m (x:xs) (c+1))
  | otherwise = masRepetidoElemento m (masRepetidoFila m (x:xs) 0)

masRepetidoVeces :: Tablero -> (Integer, Integer)
masRepetidoVeces [] = (0, 0)
masRepetidoVeces (f:fs) = masRepetidoElemento (masRepetidoFila (0, 0) (ordenar f) 0) (masRepetidoVeces fs) 

masRepetido :: Tablero -> Integer
masRepetido t = fst  (masRepetidoVeces t) 

-- permutarTablero :: Tablero -> Conjunto Tablero

-- Ejercicio 3
valoresDeCamino :: Tablero -> Camino -> [Integer]
valoresDeCamino t [] = []
valoresDeCamino t (p:ps) = (valor t p) : (valoresDeCamino t ps)

-- Ejercicio 4
caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
caminoDeCollatz t c a = esCaminoDeCollatz (valoresDeCamino t c) a

siguienteTerminoCollatz :: Integer -> Integer
siguienteTerminoCollatz a
  | mod a 2 == 0 = div a 2
  | otherwise = 3*a + 1

esCaminoDeCollatz :: [Integer] -> Integer -> Bool
esCaminoDeCollatz [] _ = True 
esCaminoDeCollatz (x:xs) a 
  | x == a = esCaminoDeCollatz xs (siguienteTerminoCollatz a)
  | otherwise = False

-- Ejercicio 5
mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatz t a = caminoMasLargo (calcularTodosCollatz t (encontrarTodasPos a))

calcularTodosCollatz :: Tablero -> [Posicion] -> [Camino]
calcularTodosCollatz t [] = []
calcularTodosCollatz t (p:ps) = (calcularCollatz t p) : (calcularTodosCollatz t ps)

calcularCollatz :: Tablero -> Posicion
calcularCollatz t p = 

encontrarTodasPos :: Tablero -> Integer -> Posicion -> [Posicion]
encontrarTodasPos t a p 
  | not posValida t p = []
  | valor t p == a = p : (encontrarTodasPos t a (avanzar p))
  | otherwise = encontrarTodasPos t a (avanzar p)   

-----------------------------------------------------------------------
-- AUX
-----------------------------------------------------------------------
tomarHasta :: [a] -> Integer -> [a]
tomarHasta [] _ = []
tomarHasta xs 0 = [] 
tomarHasta (x:xs) n = x : tomarHasta xs (n-1)

borrarHasta :: [a] -> Integer -> [a]
borrarHasta [] _ = []
borrarHasta xs 0 = xs
borrarHasta (x:xs) n = borrarHasta xs (n-1)

unir :: (Ord a) => [a] -> [a] -> [a]
unir [] xs = xs
unir xs [] = xs
unir (x:xs) (y:ys)
    | (x < y) = x:unir xs (y:ys)
    | otherwise = y:unir (x:xs) ys
 
dividirALaMitad :: [a] -> ([a], [a])
dividirALaMitad xs = (tomarHasta xs n, borrarHasta xs n)
    where n = toInteger (div (length xs) 2) 
 
ordenar :: (Ord a) => [a] -> [a]
ordenar xs 
    | (length xs) > 1 = unir (ordenar ls) (ordenar rs)
    | otherwise = xs
    where (ls, rs) = dividirALaMitad xs
