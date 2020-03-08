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


---------------------SOLUCION DEL TP----------------------------------------------------------
--1)
maximo::Tablero->Integer
maximo (f:[])= maximoDeLaFila f
maximo (f1:f2:fs)|maximoDeLaFila f1 > maximoDeLaFila f2 = maximo (f1:fs)
                 |maximoDeLaFila f1 <=maximoDeLaFila f2 = maximo (f2:fs)

maximoDeLaFila::Fila->Integer
maximoDeLaFila (n:[])= n
maximoDeLaFila (n1:n2:ns)|n1>n2 = maximoDeLaFila (n1:ns)
                         |n1<=n2= maximoDeLaFila (n2:ns) 

--2)
masRepetido::Tablero->Integer
masRepetido t = masAparecido (valores t)

valores::Tablero->[Integer]
valores (f:[])= f 
valores (f:fs)= f ++ valores fs

masAparecido::Eq a=>[a]->a
masAparecido (x:[])    = x
masAparecido (x1:x2:xs)|cantidadDeApariciones x1 (x1:x2:xs)> cantidadDeApariciones x2 (x1:x2:xs)= masAparecido (x1:xs)
                       |cantidadDeApariciones x1 (x1:x2:xs)<=cantidadDeApariciones x2 (x1:x2:xs)= masAparecido (x2:xs)

cantidadDeApariciones::(Num p,Eq a)=> a->[a]->p
cantidadDeApariciones x []          = 0
cantidadDeApariciones x (y:ys)|x==y = 1 + cantidadDeApariciones x ys
                              |x/=y =     cantidadDeApariciones x ys

--3)
valoresDeCamino::Tablero->Camino->[Integer]
valoresDeCamino t []            = []
valoresDeCamino t (p:posiciones)= valor t p : valoresDeCamino t posiciones

--4)
caminoDeCollatz::Tablero->Camino->Integer->Bool
caminoDeCollatz t c n= esSucesionDeCollats (valoresDeCamino t c) n

esSucesionDeCollats::[Integer]->Integer->Bool
esSucesionDeCollats (x:[]) n = True
esSucesionDeCollats (x1:x2:xs) n |x1==1 = False
                                 |n/=x1 = False
                                 |mod n 2==0 = (x2==div n 2) && esSucesionDeCollats (x2:xs) (div n 2)
                                 |mod n 2/=0 = (x2==3*n+1)   && esSucesionDeCollats (x2:xs) (3*n+1) 

--5)
mayorSecuenciaDeCollatz::Tablero->Integer->[Integer]
mayorSecuenciaDeCollatz t n = valoresDeCamino t (caminoDeCollatzMasLargoPorNumero t n)


secDeCollatsMasLargas::Tablero->Posicion->[Camino]
secDeCollatsMasLargas t (x,y)
        |posValida t (x+1,y) && posValida t (x,y+1) && valor t (x+1,y)== valor t (x,y+1) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatsMasLargas t (x+1,y)) ++ agregarATodas (x,y) (secDeCollatsMasLargas t (x,y+1))
        |posValida t (x+1,y) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatsMasLargas t (x+1,y))
        |posValida t (x+1,y) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x,y+1)] (valor t (x,y)) = agregarATodas (x,y) (secDeCollatsMasLargas t (x,y+1))
        |posValida t (x+1,y) && not (posValida t (x,y+1)) && caminoDeCollatz t [(x,y),(x+1,y)] (valor t (x,y))= agregarATodas (x,y) (secDeCollatsMasLargas t (x+1,y))
        |not (posValida t (x+1,y)) && posValida t (x,y+1) && caminoDeCollatz t [(x,y),(x,y+1)] (valor t (x,y))= agregarATodas (x,y) (secDeCollatsMasLargas t (x,y+1))
        |otherwise = [(x,y)]:[]

agregarATodas::a->[[a]]->[[a]]
agregarATodas _ []=[]
agregarATodas y (xs:xss)= (y:xs):agregarATodas y xss 


generarTuplasDeUnaFila::Integer->Integer->[(Integer,Integer)]
generarTuplasDeUnaFila n 1 = (n,1):[]
generarTuplasDeUnaFila n m = (n,m):generarTuplasDeUnaFila n (m-1)

generarTuplas::Integer->Integer->[Posicion]
generarTuplas 1 m = generarTuplasDeUnaFila 1 m
generarTuplas n m = generarTuplasDeUnaFila n m ++ generarTuplas (n-1) m

posiciones::Tablero->[Posicion]
posiciones t = generarTuplas (cantidadFilas t) (cantidadColumnas t)

posicionesIgualesAUnNumero::Tablero->[Posicion]->Integer->[Posicion]
posicionesIgualesAUnNumero t [] n = []
posicionesIgualesAUnNumero t (p:posiciones) n |valor t p /= n = posicionesIgualesAUnNumero t posiciones n 
                                              |valor t p == n = p:posicionesIgualesAUnNumero t posiciones n

todasPosIgualesAUnNumero::Tablero->Integer->[Posicion]
todasPosIgualesAUnNumero t n = posicionesIgualesAUnNumero t (posiciones t) n 

secCollatsAPartirDeTuplas::Tablero->[Posicion]->[Camino]
secCollatsAPartirDeTuplas t []      = [] 
secCollatsAPartirDeTuplas t (p:posicion)= secDeCollatsMasLargas t p ++ secCollatsAPartirDeTuplas t posicion

secCollatsMasLargasPorNumero::Tablero->Integer->[Camino]
secCollatsMasLargasPorNumero t n = secCollatsAPartirDeTuplas t (todasPosIgualesAUnNumero t n)

caminoMasLargo::[Camino]->Camino
caminoMasLargo []     = []
caminoMasLargo (c:[]) = c
caminoMasLargo (c1:c2:caminos)|fromIntegral (length c1) > fromIntegral (length c2) = caminoMasLargo (c1:caminos)
                              |otherwise             = caminoMasLargo (c2:caminos) 

caminoDeCollatzMasLargoPorNumero::Tablero->Integer->Camino
caminoDeCollatzMasLargoPorNumero t n = caminoMasLargo (secCollatsMasLargasPorNumero t n)


pertenece::Eq a => a->[a]->Bool
pertenece x xs | xs==[]        = False
               | otherwise     = x==head xs || pertenece x (tail xs)

--6)
mayorSecuenciaDeCollatzPermutado::Tablero->Integer->[Integer]
mayorSecuenciaDeCollatzPermutado t n = listaMasGrande (mayoresCollatzPermutados t n)

mayoresCollatzListaDeTableros::[Tablero]->Integer->[[Integer]]
mayoresCollatzListaDeTableros (t:[]) n       = mayorSecuenciaDeCollatz t n :[]
mayoresCollatzListaDeTableros (t:tableros) n = mayorSecuenciaDeCollatz t n : mayoresCollatzListaDeTableros tableros n

mayoresCollatzPermutados::Tablero->Integer->[[Integer]]
mayoresCollatzPermutados t n = mayoresCollatzListaDeTableros (permutaciones t) n

listaMasGrande::[[a]]->[a]
listaMasGrande (xs:[]) = xs
listaMasGrande (xs1:xs2:xss)|length xs1 >length xs2 = listaMasGrande (xs1:xss)
                            |length xs1 <=length xs2 = listaMasGrande (xs2:xss)

insertarEnPos :: Fila -> [Fila] -> Integer -> [Fila]
insertarEnPos f xs 1 = f:xs
insertarEnPos f (x:xs) k = x:(insertarEnPos f xs (k-1))

insertarEnTodaPosHasta :: Fila -> [Fila] -> Integer -> [[Fila]]
insertarEnTodaPosHasta n xs 1 = insertarEnPos n xs 1 : []
insertarEnTodaPosHasta n xs k = insertarEnPos n xs k : insertarEnTodaPosHasta n xs (k-1) 

insertarEnTodaPosicion :: Fila -> [Fila] -> [[Fila]]
insertarEnTodaPosicion n xs = insertarEnTodaPosHasta n xs (fromIntegral (length xs) + 1)

insertarEnTodaListaEnTodaPos :: Fila -> [[Fila]] -> [[Fila]]
insertarEnTodaListaEnTodaPos n [] = []
insertarEnTodaListaEnTodaPos n (x:cxs) = insertarEnTodaPosicion n x ++ insertarEnTodaListaEnTodaPos n cxs

permutaciones :: Tablero -> [Tablero]
permutaciones (f:[]) = [[f]]
permutaciones (f:filas) = insertarEnTodaListaEnTodaPos f (permutaciones filas)

