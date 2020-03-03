-----------------------------------------------------------------------
-- TALLER DE ALGEBRA I
-- Verano 2020

-- NUMERO DE GRUPO: 

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1:
-- INTEGRANTE 2:
-----------------------------------------------------------------------
module SolucionTP
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

