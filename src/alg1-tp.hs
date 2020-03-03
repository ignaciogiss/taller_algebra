-----------------------------------------------------------------------
-- TALLER DE ALGEBRA I
-- Verano 2020

-- NUMERO DE GRUPO: 

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1:
-- INTEGRANTE 2:
-----------------------------------------------------------------------
module SolucionTP
( maximo )
where 

import Conjuntos

-- type Conjunto a = [a]
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

contarRepeticiones :: Integer -> Fila -> Integer -> (Integer, Integer)
contarRepeticiones _ [] c = c
contarRepeticiones y (x:xs) c
  | y == x = contarRepeticiones y xs (c+1)
  | otherwise = contarRepeticiones y xs c

contar :: Integer -> Fila -> (Integer, Integer)
contar y (x:xs) = contarRepeticiones y (x:xs) 1

masRepetidoElemento :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
masRepetidoElemento (x, cx) (y, cy)
  | cx >= cy = (x, cx)
  | otherwise = (y, cy)

masRepetidoFila :: Fila -> Integer -> (Integer, Integer)
masRepetidoFila (x:xs) c = masRepetidoElemento (contar x xs) (masRepetidoFila xs)

masRepetidoVeces :: Tablero -> (Integer, Integer)
masRepetidoVeces [] = 0
masRepetidoVeces (f:fs) = masRepetidoElemento (masRepetidoFila f) (masRepetidoVeces fs)

masRepetido :: Tablero -> Integer
masRepetido t = fst (masRepetidoVeces t)


permutarTablero :: Tablero -> Conjunto Tablero
