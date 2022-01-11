module Repaso where

import           Data.Char
import           Data.Maybe

-- 1. Orden superior
----------------------------------------------------------------

-- |
-- >>> twice (+1) 5
-- 7
--
-- >>> twice (*2) 3
-- 12

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- |
-- >>> mapTuple (+1) (*2) (3, 4)
-- (4,8)
--
-- >>> mapTuple ord (==5) ('A', 3 + 1)
-- (65,False)

mapTuple :: (a->c) -> (b->d) -> (a,b) -> (c,d)
mapTuple f g (x,y) = (f x, g y)

-- map, filter
-- lambda expresiones
-- secciones

-- |
-- >>> aprobadoGeneral [1..10]
-- [5.0,5.0,5.0,5.0,5.0,6.0,7.0,8.0,9.0,10.0]
--
-- >>> aprobadoGeneral [4.7, 2.5, 7, 10, 8.7]
-- [5.0,5.0,7.0,10.0,8.7]

aprobadoGeneral :: [Double] -> [Double]
aprobadoGeneral xs = map (max 5) xs --map (\x-> if x<5 then 5 else x) xs 
-- eta reduccion  aprobadoGeneral = map (max 5) 

-- MAD = Multiply, Add, Divide

-- |
-- >> f = mad 2 3 7
-- >> f 5
-- 6
-- >> f 10
-- 2

mad :: Int -> Int -> Int -> (Int -> Int)
mad m a d = \n -> mod ((n*m)+a) d

-- 2. Plegado de listas
----------------------------------------------------------------

-- revisión de la recursión sobre listas

-- |
-- >>> suma [1..10]
-- 55
--
-- >>> suma [7]
-- 7
--
-- >>> suma []
-- 0

suma :: Num a => [a] -> a
suma []     = 0
suma (x:xs) = x + suma xs

sumaRR :: Num a => [a] -> a
sumaRR [] = 0
sumaRR (x:xs) = (+) x (sumaRR xs)
-- |
-- >>> longitud "hola mundo"
-- 10
--
-- >>> longitud [True]
-- 1
--
-- >>> longitud []
-- 0

longitud :: [a] -> Integer
longitud []     = 0
longitud (_:xs) = 1 + longitud xs

longitudRR :: [a] -> Integer
longitudRR [] = 0
longitudRR (x:xs) = f x (longitudRR xs)
  where
      f _ solCola = solCola + 1
-- |
-- >>> conjunción [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunción [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunción []
-- True

conjunción :: [Bool] -> Bool
conjunción [] = True
conjunción (x:xs) = x && conjunción xs

conjunciónRR :: [Bool] -> Bool
conjunciónRR [] = True
conjunciónRR (x:xs) = (&&) x (conjunciónRR xs)
-- |
-- >>> esPalabra "haskell"
-- True
--
-- >>> esPalabra "haskell 2017"
-- False
--
-- >>> esPalabra "h"
-- True
--
-- >>> esPalabra ""
-- True

esPalabra :: String -> Bool
esPalabra "" = True
esPalabra (x:xs) = isAlpha x && esPalabra xs

esPalabraRR :: String -> Bool
esPalabraRR [] = True
esPalabraRR (x:xs) = f x (esPalabraRR xs)
  where
      f cabeza solCola = isAlpha cabeza && solCola
-- |
-- >>> todasMayúsculas "WHILE"
-- True
--
-- >>> todasMayúsculas "While"
-- False
--
-- >>> todasMayúsculas ""
-- True

todasMayúsculas :: String -> Bool
todasMayúsculas [] = True
todasMayúsculas (x:xs) = isUpper x && todasMayúsculas xs


-- |
-- >>> máximo "hola mundo"
-- 'u'
--
-- >>> máximo [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximo [-8]
-- -8

máximo :: Ord a => [a] -> a
máximo [x] = x
máximo (x:xs) = max x (máximo xs)

-- |
-- >>> mínimoYmáximo "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximo [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximo [1]
-- (1,1)

mínimoYmáximo :: Ord a => [a] -> (a,a)
mínimoYmáximo [x] = (x,x)
mínimoYmáximo (x:xs) = (min x currentMin, max x currentMax)
  where (currentMin, currentMax) = mínimoYmáximo xs

-- |
-- >>> aplana [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplana [[1,2]]
-- [1,2]
--
-- >>> aplana []
-- []

aplana :: [[a]] -> [a]
aplana [] = []
aplana (xs:xss) = xs ++ aplana xss

aplanaRR :: [[a]] -> [a]
aplanaRR [] = []
aplanaRR (xs:xss) = (++) xs (aplanaRR xss)

recursionLista :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
recursionLista f base [] = base
recursionLista f base (x:xs) = f x (recursionLista f base xs)

recLista :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2 -- foldr practicamente
recLista f base = recursion -- quitamos xs por eta reduccion
  where 
      recursion [] = base
      recursion (y:ys) = f y (recursion ys)

-- deducir el patrón de foldr

-- resolver las anteriores funciones con foldr

-- |
-- >>> sumaR [1..10]
-- 55
--
-- >>> sumaR [7]
-- 7
--
-- >>> sumaR []
-- 0

sumaR :: Num a => [a] -> a
sumaR = foldr (+) 0

-- |
-- >>> longitudR "hola mundo"
-- 10
--
-- >>> longitudR [True]
-- 1
--
-- >>> longitudR []
-- 0

longitudR :: [a] -> Integer
longitudR = foldr (const (+1)) 0 -- longitudR = foldr (\_ -> (+) 1) 0

-- |
-- >>> conjunciónR [1 == 1, 'a' < 'b', null []]
-- True
--
-- >>> conjunciónR [1 == 1, 'a' < 'b', null [[]]]
-- False
--
-- >>> conjunciónR []
-- True

conjunciónR :: [Bool] -> Bool
conjunciónR = foldr (&&) True

-- |
-- >>> esPalabraR "haskell"
-- True
--
-- >>> esPalabraR "haskell 2017"
-- False
--
-- >>> esPalabraR "h"
-- True
--
-- >>> esPalabraR ""
-- True

esPalabraR :: String -> Bool
esPalabraR = foldr (\cab -> (&&) (isAlpha cab)) True


esPalabraR'' :: String -> Bool
esPalabraR'' = foldr ((&&) . isAlpha) True

esPalabraR' :: String -> Bool
esPalabraR' = foldr f True
  where
      f cab solCola = isAlpha cab && solCola

-- |
-- >>> todasMayúsculasR "WHILE"
-- True
--
-- >>> todasMayúsculasR "While"
-- False
--
-- >>> todasMayúsculasR ""
-- True

todasMayúsculasR :: String -> Bool
todasMayúsculasR = foldr ((&&) . isUpper) True

-- |
-- >>> máximoR "hola mundo"
-- 'u'
--
-- >>> máximoR [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximoR [-8]
-- -8

-- Maybe y fromMaybe
máximoR :: Ord a => [a] -> a
máximoR xs = fromMaybe (error "no hay maximo") (foldr f Nothing xs)
  where
    f cabeza Nothing = Just cabeza
    f cabeza (Just x) = Just (max cabeza x)
-- |
-- >>> mínimoYmáximoR "hola mundo"
-- (' ','u')
--
-- >>> mínimoYmáximoR [7, -8, 56, 17, 34, 12]
-- (-8,56)
--
-- >>> mínimoYmáximoR [1]
-- (1,1)

mínimoYmáximoR :: Ord a => [a] -> (a,a)
mínimoYmáximoR xs = undefined

-- |
-- >>> aplanaR [[1,2], [3,4,5], [], [6]]
-- [1,2,3,4,5,6]
--
-- >>> aplanaR [[1,2]]
-- [1,2]
--
-- >>> aplanaR []
-- []

aplanaR :: [[a]] -> [a]
aplanaR = foldr (++) []

-- otros ejercicios de foldr

-- |
-- >>> mapR (2^) [0..10]
-- [1,2,4,8,16,32,64,128,256,512,1024]
--
-- >>> mapR undefined []
-- []
--
-- >>> mapR ord  "A"
-- [65]

mapR :: (a -> b) -> [a] -> [b]
mapR f = foldr (\x xs -> f x:xs) []

-- |
-- >>> filter even [1..20]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> filter undefined []
-- []
--
-- >>> filter even [5]
-- []

filterR :: (a -> Bool) -> [a] -> [a]
filterR p = foldr (\x xs -> if p x then x:xs else xs) [] 

-- |
-- >>> apariciones 'a' "casa"
-- 2
-- >>> apariciones 'u' "casa"
-- 0

apariciones :: Eq a => a -> [a] -> Integer
apariciones x xs = undefined

-- |
-- >>> purgar "abracadabra"
-- "cdbra"
--
-- >>> purgar [1,2,3]
-- [1,2,3]
--
-- >>> purgar "aaaaaaaaaa"
-- "a"

purgar :: Eq a => [a] -> [a]
purgar xs = undefined

-- |
-- >>> agrupa "mississippi"
-- ["m","i","ss","i","ss","i","pp","i"]
--
-- >>> agrupa [1,2,2,3,3,3,4,4,4,4]
-- [[1],[2,2],[3,3,3],[4,4,4,4]]
--
-- >>> agrupa []
-- []

agrupa :: Eq a => [a] -> [[a]]
--agrupa [] = []
--agrupa (x:xs) = (x : takeWhile (==x) xs) : agrupa (dropWhile (==x) xs)
agrupa = foldr f []
  where f x []        = [[x]]
        f x (ys@(y:_):yss)
          | x == y    = (x:ys):yss
          | otherwise = [x]:ys:yss

-- 3. Plegado de tipos algebraicos recursivos
----------------------------------------------------------------

data Tree a = Empty
            | Leaf a
            | Node a (Tree a) (Tree a)
            deriving Show

treeI :: Tree Integer
treeI = Node 1
             (Node 2 (Leaf 4) (Leaf 5))
             (Node 3 Empty (Leaf 6))

treeC :: Tree Char
treeC = Node 'z'
          (Node 't' (Node 's' Empty (Leaf 'a')) (Leaf 'g'))
          (Node 'w' (Leaf 'h') (Node 'p' (Leaf 'f') (Leaf 'n')))

-- |
-- >>> treeSize treeI
-- 6
--
-- >>> treeSize treeC
-- 10

treeSize :: Tree a -> Integer
treeSize Empty        = 0 -- constante base 1
treeSize (Leaf x)     = (const 1) x -- funcion base 2
treeSize (Node x l r) = f x (treeSize l) (treeSize r)
  where 
      f _ solL solR = 1 + solL + solR
-- |
-- >>> treeHeight treeI
-- 3
-- >>> treeHeight treeC
-- 4

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Leaf _) = 1
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- |
-- >>> treeSum treeI
-- 21

treeSum :: Num a => Tree a -> a
treeSum Empty        = 0 -- c base 1
treeSum (Leaf x)     = id x -- f base 2 funcion identidad en Haskell id: (f x = x)
treeSum (Node x l r) = f x (treeSum l) (treeSum r)
  where 
      f raiz solL solR = raiz + solL + solR

-- |
-- >>> treeProduct treeI
-- 720

treeProduct :: Num a => Tree a -> a
treeProduct Empty = 1
treeProduct (Leaf x) = id x
treeProduct (Node x l r) = f x (treeSum l) (treeSum r)
  where
    f raiz solL solR = raiz * solL * solR

-- |
-- >>> treeElem 5 treeI
-- True
--
-- >>> treeElem 48 treeI
-- False
--
-- >> treeElem 'w' treeC
-- True
--
-- >>> treeElem '*' treeC
-- False

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Empty        = False
treeElem x (Leaf y)     = (== y) x
treeElem x (Node y l r) = f x (treeElem x l) (treeElem x r)
  where
    f raiz solI solR = raiz==y || solI || solR

-- |
-- >>> treeToList treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList treeC
-- "satgzhwfpn"

treeToList :: Tree a -> [a]
treeToList Empty        = []
treeToList (Leaf x)     = (:[]) x      -- [x]
treeToList (Node x lt rt) = f x (treeToList lt) (treeToList rt)
  where
    f raiz solI solD = solI ++ raiz : solD

-- |
-- >>> treeBorder treeI
-- [4,5,6]
--
-- >>> treeBorder treeC
-- "aghfn"

treeBorder :: Tree a -> [a]
treeBorder Empty = []  -- base0 - constante
treeBorder (Leaf x) = (:[]) x  -- base1 - funcion sobre hoja 
treeBorder (Node _ l r) = f undefined (treeBorder l) (treeBorder r)  -- f raiz solI solD
  where
    f _ solL solR = solL ++ solR

-- introducir el plegado del tipo Tree a
foldTree :: (a->b->b->b) -> (a->b) -> b -> Tree a -> b
foldTree n l solEmpty = recTree
  where
    recTree Empty = solEmpty
    recTree (Leaf x) = l x
    recTree (Node r lt rt) =  n r (recTree lt) (recTree rt)

-- resolver los ejercicios anteriores con foldTree

-- |
-- >>> treeSize' treeI
-- 6
--
-- >>> treeSize' treeC
-- 10

treeSize' :: Tree a -> Integer
treeSize' = foldTree (\ _ si sd -> 1 + si + sd) (const 1) 0

-- |
-- >>> treeHeight' treeI
-- 3
-- >>> treeHeight' treeC
-- 4

treeHeight' :: Tree a -> Integer
treeHeight' = foldTree (\ _ si sd -> 1 + max si sd) (const 1) 0

-- |
-- >>> treeSum' treeI
-- 21

treeSum' :: Num a => Tree a -> a
treeSum' = foldTree (\ r si sd -> r + si+ sd) id 0

-- |
-- >>> treeProduct' treeI
-- 720

treeProduct' :: Num a => Tree a -> a
treeProduct' = undefined

-- |
-- >>> treeElem' 5 treeI
-- True
--
-- >>> treeElem' 48 treeI
-- False
--
-- >> treeElem' 'w' treeC
-- True
--
-- >>> treeElem' '*' treeC
-- False

treeElem' :: Eq a => a -> Tree a -> Bool
treeElem' x = foldTree (\ r si sd -> r == x || si || sd) (==x) False

-- |
-- >>> treeToList' treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList' treeC
-- "satgzhwfpn"

treeToList' :: Tree a -> [a]
treeToList' = foldTree (\ r si sd -> si ++ r : sd) (:[]) []

-- |
-- >>> treeBorder' treeI
-- [4,5,6]
--
-- >>> treeBorder' treeC
-- "aghfn"

treeBorder' :: Tree a -> [a]
treeBorder' = undefined

--

treeMaximum :: Ord a => Tree a -> a
treeMaximum = undefined

treeMaximum' :: Ord a => Tree a -> a
treeMaximum' = undefined
