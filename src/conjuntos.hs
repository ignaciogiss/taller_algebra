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
permutaciones :: Integer -> Conjunto [Integer]
permutaciones 1 = agregar [1] vacio
permutaciones n = insertarEnTodaListaEnTodaPos n (permutaciones (n-1))

insertarEnTodaPosHasta :: Integer -> [Integer] -> Integer -> Conjunto [Integer]
insertarEnTodaPosHasta n xs 1 = agregar (n:xs) vacio
insertarEnTodaPosHasta n xs k = agregar (insertarEnPos n xs k) (insertarEnTodaPosHasta n xs (k-1)) 


insertarEnPos :: Integer -> [Integer] -> Integer -> [Integer]
insertarEnPos n xs 1 = n:xs
insertarEnPos n (x:xs) k = x:(insertarEnPos n xs (k-1))

insertarEnTodaListaEnTodaPos :: Integer -> Conjunto [Integer] -> Conjunto [Integer]
-- insertarEnTodaListaEnTodaPos n (xs:[]) = 
insertarEnTodaListaEnTodaPos n xs = insertarEnTodaPosHasta n xs (length xs + 1) 

insertarEnTodaPosicion :: Integer -> [Integer] -> Conjunto [Integer]
insertarEnTodaPosicion n (xs:cxs) = union (insertarEnTodaPosicion n xs) (insertarEnTodaListaEnTodaPos n cxs)

