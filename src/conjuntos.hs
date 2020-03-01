module Conjuntos
where

type Conjunto a = [a]
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
-- insertarEnTodaPosicion n (xs:cxs) = union (insertarEnTodaPosicion n xs) (insertarEnTodaListaEnTodaPos n cxs)
insertarEnTodaPosicion n xs = insertarEnTodaPosHasta n xs (toInteger(length xs) + 1) 

