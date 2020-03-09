-----------------------------------------------------------------------
-- TALLER DE ALGEBRA I
-- Verano 2020

-- NUMERO DE GRUPO: 2 

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Ramírez, Leandro     LU:  90/09
-- INTEGRANTE 2: Cardozo, Alfredo     LU: 820/19
-- INTEGRANTE 3: Giss, Rubén Ignacio  LU: 176/07
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
-- SOLUCION DEL TP
-----------------------------------------------------------------------
--1)
maximo::Tablero->Integer
maximo (f:[])= maximoDeLaFila f
maximo (f1:f2:fs)|maximoDeLaFila f1 > maximoDeLaFila f2 = maximo (f1:fs)
                 |maximoDeLaFila f1 <=maximoDeLaFila f2 = maximo (f2:fs)

maximoDeLaFila::Fila->Integer
maximoDeLaFila (n:[])= n
maximoDeLaFila (n1:n2:ns)|n1>n2 = maximoDeLaFila (n1:ns)
                         |n1<=n2= maximoDeLaFila (n2:ns) 

-----------------------------------------------------------------------
--2)
masRepetido :: Tablero -> Integer
masRepetido ts = devuelveElMasRepetido(concatenar ts)

-- convierte el tablero en una lista
concatenar :: Tablero -> [Integer]
concatenar (x:[]) = x
concatenar (x:xs) = x ++ concatenar xs    

-- dada una lista devuelve el mas repetido
devuelveElMasRepetido :: [Integer] -> Integer 
devuelveElMasRepetido xs = devuelvefstDadosnd (maximosndDeLista (listDeApariciones xs)) (listDeApariciones xs)

-- dado snd y una list de tuplas devuelve fst para ese snd de esa tupla
devuelvefstDadosnd :: Integer -> [(Integer,Integer)] -> Integer
devuelvefstDadosnd n (x:[]) = fst x
devuelvefstDadosnd n (x:xs) | n == snd x = fst x
                            | otherwise = (devuelvefstDadosnd n xs) 

-- dada una lista de enteros (x:xs) devuelve una lista de pares.
-- donde la primer coordenada del par es un elemento de la lista y la segunda coordenada es 
-- la cantidad de veces que aparaece en la lista. 
listDeApariciones :: [Integer] -> [(Integer,Integer)]
listDeApariciones [] = []
listDeApariciones (x : []) = [(x,1)] 
listDeApariciones (x:xs) = (x,cantApariciones x xs + 1)  : listDeApariciones ( quitarTodaAparicion x (xs) ) 

-- quita todas las apariciones de un entero de una lista
quitarTodaAparicion :: Integer -> [Integer] -> [Integer]
quitarTodaAparicion n xs | length xs == 0 = []
                         | length xs > 0 && head xs /= n = head xs : [] ++ quitarTodaAparicion n (tail xs)
                         | otherwise = quitarTodaAparicion n (tail xs)

-- de una lista de tuplas busca el maximo snd
maximosndDeLista :: [(Integer,Integer)] -> Integer
maximosndDeLista (x:[]) = snd x
maximosndDeLista (x:xs) = max (snd x) (maximosndDeLista xs) 

-- Devuelve la cantidad de apariciones de un entero en una lista
cantApariciones :: Integer -> [Integer] -> Integer
cantApariciones n [] = 0
cantApariciones n (x:xs) | n == x = 1 + cantApariciones n xs  
                         | otherwise = cantApariciones n xs 

-----------------------------------------------------------------------
--3)
valoresDeCamino::Tablero->Camino->[Integer]
valoresDeCamino t []            = []
valoresDeCamino t (p:posiciones)= valor t p : valoresDeCamino t posiciones

-----------------------------------------------------------------------
--4)
caminoDeCollatz::Tablero->Camino->Integer->Bool
caminoDeCollatz _ [] _ = True
caminoDeCollatz t c n= esSucesionDeCollatz (valoresDeCamino t c) n

-- Dada una lista y un valor de inicializacion de la sucesion de Collatz,
-- devuelve True si todos los elementos pertenecen a la sucesion de Collatz inicializada
-- con el valor dado, o False en caso contrario.
esSucesionDeCollatz::[Integer]->Integer->Bool
esSucesionDeCollatz (x:[]) n = True
esSucesionDeCollatz (x1:x2:xs) n |x1==1 = False
                                 |n/=x1 = False
                                 |mod n 2==0 = (x2==div n 2) && esSucesionDeCollatz (x2:xs) (div n 2)
                                 |mod n 2/=0 = (x2==3*n+1)   && esSucesionDeCollatz (x2:xs) (3*n+1) 

-----------------------------------------------------------------------
--5)
mayorSecuenciaDeCollatz::Tablero->Integer->[Integer]
mayorSecuenciaDeCollatz t n = valoresDeCamino t (caminoDeCollatzMasLargoPorNumero t n)

-- Calcula el camino mas largo de Collatz para un tablero y un valor de inicializacion dado.
caminoDeCollatzMasLargoPorNumero::Tablero->Integer->Camino
caminoDeCollatzMasLargoPorNumero t n = caminoMasLargo (secCollatzMasLargasPorNumero t n)

-- Devuelve todos los caminos de Collatz para un tablero y un valor de inicializacion.
secCollatzMasLargasPorNumero::Tablero->Integer->[Camino]
secCollatzMasLargasPorNumero t n = secCollatzAPartirDeTuplas t (todasPosIgualesAUnNumero t n)

-- Devuelve todas las posiciones del tablero t cuyo valor es igual a n.
todasPosIgualesAUnNumero::Tablero->Integer->[Posicion]
todasPosIgualesAUnNumero t n = posicionesIgualesAUnNumero t (posiciones t) n 

-- Devuelve una lista con todas las posiciones de un tablero. 
posiciones::Tablero->[Posicion]
posiciones t = generarTuplas (cantidadFilas t) (cantidadColumnas t)

-- Dados dos valores, n y m, genera una lista con todas las posiciones formadas
-- por todos los valores de 1 hasta n en la primer coordenada y de 1 hasta m en la segunda.
-- Es utilizada para generar las posiciones de un tablero 
-- con n <= cantidad de filas y m <= cantidad de columnas.
generarTuplas::Integer->Integer->[Posicion]
generarTuplas 1 m = generarTuplasDeUnaFila 1 m
generarTuplas n m = generarTuplasDeUnaFila n m ++ generarTuplas (n-1) m

--  Dados dos valores, n y m, genera una lista con todas las posiciones para una fila dada por n.
generarTuplasDeUnaFila::Integer->Integer->[(Integer,Integer)]
generarTuplasDeUnaFila n 1 = (n,1):[]
generarTuplasDeUnaFila n m = (n,m):generarTuplasDeUnaFila n (m-1)

-- Dado un tablero t, una lista de posiciones (p:posiciones) y un valor n
-- Devuelve todas las posiciones que tienen el valor n.
posicionesIgualesAUnNumero::Tablero->[Posicion]->Integer->[Posicion]
posicionesIgualesAUnNumero t [] n = []
posicionesIgualesAUnNumero t (p:posiciones) n |valor t p /= n = posicionesIgualesAUnNumero t posiciones n 
                                              |valor t p == n = p:posicionesIgualesAUnNumero t posiciones n

-- Devuelve todos los caminos mas largos de Collatz a partir una lista de posiciones de inicializacion.
secCollatzAPartirDeTuplas::Tablero->[Posicion]->[Camino]
secCollatzAPartirDeTuplas t []      = [] 
secCollatzAPartirDeTuplas t (p:posicion)= secDeCollatzMasLargas t p ++ secCollatzAPartirDeTuplas t posicion


-- Devuelve todos los caminos de Collatz partiendo de una posicion.
secDeCollatzMasLargas::Tablero->Posicion->[Camino]
secDeCollatzMasLargas t (x,y)
        |posValida t (x+1,y) && posValida t (x,y+1) && valor t (x+1,y)== valor t (x,y+1) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatzMasLargas t (x+1,y)) ++ agregarATodas (x,y) (secDeCollatzMasLargas t (x,y+1))
        |posValida t (x+1,y) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatzMasLargas t (x+1,y))
        |posValida t (x+1,y) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x,y+1)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatzMasLargas t (x,y+1))
        |posValida t (x+1,y) && not (posValida t (x,y+1)) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y))= agregarATodas (x,y) (secDeCollatzMasLargas t (x+1,y))
        |not (posValida t (x+1,y)) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x,y+1)] (valor t (x,y))= agregarATodas (x,y) (secDeCollatzMasLargas t (x,y+1))
        |otherwise = [(x,y)]:[]

-- Agrega el elemnto y a todas las listas xs de una lista de listas (xs:xss).
agregarATodas::a->[[a]]->[[a]]
agregarATodas _ []=[]
agregarATodas y (xs:xss)= (y:xs):agregarATodas y xss 

-- Dada una lista de caminos, devuelve el camino mas largo.
caminoMasLargo::[Camino]->Camino
caminoMasLargo []     = []
caminoMasLargo (c:[]) = c
caminoMasLargo (c1:c2:caminos)|fromIntegral (length c1) > fromIntegral (length c2) = caminoMasLargo (c1:caminos)
                              |otherwise             = caminoMasLargo (c2:caminos) 

-----------------------------------------------------------------------
--6)
mayorSecuenciaDeCollatzPermutando::Tablero->Integer->[Integer]
mayorSecuenciaDeCollatzPermutando t n = listaMasGrande (mayoresCollatzPermutados t n)

-- Dado un tablero t y un valor de inicializacion n
-- devuelve una lista con todas las mayores secuencias de Collatz. 
mayoresCollatzPermutados::Tablero->Integer->[[Integer]]
mayoresCollatzPermutados t n = mayoresCollatzListaDeTableros (permutaciones t) n

-- Dada un conjunto de tableros y un valor de inicializacion n devuelve una lista con
-- todas las mayores secuencias de Collatz.
mayoresCollatzListaDeTableros :: Conjunto Tablero -> Integer -> [[Integer]]
mayoresCollatzListaDeTableros (t:[]) n       = mayorSecuenciaDeCollatz t n :[]
mayoresCollatzListaDeTableros (t:tableros) n = mayorSecuenciaDeCollatz t n : mayoresCollatzListaDeTableros tableros n

-- Dada una lista de listas devuelve la lista con mayor cantidad de elementos.
listaMasGrande::[[a]]->[a]
listaMasGrande (xs:[]) = xs
listaMasGrande (xs1:xs2:xss)|length xs1 >length xs2 = listaMasGrande (xs1:xss)
                            |length xs1 <=length xs2 = listaMasGrande (xs2:xss)

-- Obtener todas las permutaciones de un Tablero.
permutaciones :: Tablero -> [Tablero]
permutaciones (f:[]) = [[f]]
permutaciones (f:filas) = insertarEnTodaListaEnTodaPos f (permutaciones filas)

-- Inserta el elemento n en en todas las listas del conjunto xs en todas las posiciones
-- Ej. [[f1 f2] [f2 f1]] -> [[n f1 f2] [f1 n f2] [f1 f2 n] [n f2 f1] [f2 n f1] [f2 f1 n]]
-- Si la cantidad de elementos de xs es k, entonces la cantidad de elementos retornados
-- es k*(k+1).
insertarEnTodaListaEnTodaPos :: Fila -> [[Fila]] -> Conjunto Tablero
insertarEnTodaListaEnTodaPos n [] = vacio 
insertarEnTodaListaEnTodaPos n (x:cxs) = union (insertarEnTodaPosicion n x) (insertarEnTodaListaEnTodaPos n cxs)

-- Inserta n en toda posicion de la lista xs.
insertarEnTodaPosicion :: Fila -> [Fila] -> Conjunto Tablero 
insertarEnTodaPosicion n xs = insertarEnTodaPosHasta n xs (fromIntegral (length xs) + 1)

-- Inserta n en toda posicion de la lista xs hasta la posicion k.
insertarEnTodaPosHasta :: Fila -> [Fila] -> Integer -> Conjunto Tablero
insertarEnTodaPosHasta n xs 1 = agregar (insertarEnPos n xs 1) vacio 
insertarEnTodaPosHasta n xs k = agregar (insertarEnPos n xs k) (insertarEnTodaPosHasta n xs (k-1))

-- Inserta n en la lista dada (x:xs) en la posicion k.
insertarEnPos :: Fila -> [Fila] -> Integer -> [Fila]
insertarEnPos f xs 1 = f:xs
insertarEnPos f (x:xs) k = x:(insertarEnPos f xs (k-1))


-----------------------------------------------------------------------
-- CONJUNTOS
-----------------------------------------------------------------------
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

union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union [] cy = cy
union (x:cx) cy = agregar x (union cx cy)

interseccion :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccion [] _ = []
interseccion (x:cx) cy
  | pertenece x cy = agregar x ( interseccion cx cy)
