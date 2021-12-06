----------------------------------------------------------------
-- Lenguajes de Programación
-- 4 del Grado en Ingeniería Informática, mención en Computación
-- Pablo López
----------------------------------------------------------------

module Repaso where

import           Data.Char
import           Data.List
import           Data.Maybe
import GHC.Integer.GMP.Internals (BigNat)

-- Haskell = Puro + Tipado estático fuerte + Perezoso

-- | 1. Tipos básicos:
----------------------------------------------------------------

-- Los nombres de los tipos empiezan por mayúsculas:
-- Int, Integer, Float, Double, Char, Bool

-- | 2. Definición de funciones: currificación
----------------------------------------------------------------

-- square

{-

   int square(int x) {
      return x * x;
   }

-}

square :: Integer -> Integer -- declaración
square x = x * x             -- definición

-- pythagoras

{-

   int pythagoras(int x, int y) {
      return square(x) + square(y);
   }

-}

pythagoras :: Integer -> Integer -> Integer
pythagoras x y = square x + square y

-- | 3. Condicionales en Haskell
----------------------------------------------------------------

-- El condicional if then else en Haskell:
--
--     1) es una expresión, no una sentencia;
--     2) el else es obligatorio
--     3) los tipos de then y else debe coincidir

maxOf :: Integer -> Integer -> Integer -- predefinida como max
maxOf x y = if x >= y then x else y

-- signo (predefinida como signum)

{-

   int signo(int x) {
      if (x == 0) return  0;
      if (x <  0) return -1;
      if (x >  0) return  1;
   }

-}

-- |
-- >>> signo 6
-- 1
-- >>> signo 0
-- 0
-- >>> signo (-3)
-- -1

signo :: Integer -> Integer  -- predefinida como signum
signo x  | x == 0 = 0
         | x < 0  = -1
         | x > 0  = 1

-- Es mejor emplear guardas que anidar if then else
-- guarda ::= '|' exp_Bool = exp

maxOf' :: Integer -> Integer -> Integer
maxOf' x y
    | x >= y = x
    | otherwise = y

signo' :: Integer -> Integer
signo' x = undefined

-- Curiosidad: las guardas de una ecuación no tienen que ser exhaustivas;
-- las guardas de una función (que puede tener varias ecuaciones) sí deben
-- ser exhaustivas.
-- Utilizaremos mucho estre truco para implementar las semánticas.

múltiploDe2o5 x
   | x `mod` 2 == 0 = "múltiplo de 2"

múltiploDe2o5 x
   | x `mod` 5 == 0 = "múltiplo de 5"

múltiploDe2o5 x = "no es múltiplo ni de 2 ni de 5"

-- | 4. Tuplas
----------------------------------------------------------------

--       valor                    tipo
-- ('a', 1 <= 2, ord 'A') :: (Char, Bool, Int)

-- el número de componentes es fijo
-- cada componente puede ser de un tipo distinto
-- es un producto cartesiano
-- existe la tupla vacía: ()
-- no existe la tupla unitaria: (x)
-- utilidad de las tuplas: una función puede devolver varios datos

predSuc :: Integer -> (Integer, Integer)
predSuc x = (x-1, x+1)

-- | 5. Parámetros formales y patrones
----------------------------------------------------------------

-- un parámetro formal es un patrón donde debe encajar el parámetro real

-- patrón variable: x
módulo :: (Integer, Integer) -> Integer
módulo x = square (fst x) + square (snd x)

-- patrón tupla: (x,y)
módulo' :: (Integer, Integer) -> Integer
módulo' (x, y) = square x + square y

esOrigen :: (Integer, Integer) -> Bool
esOrigen x = fst x == 0 && snd x == 0

-- patrón constante: 0
esOrigen' :: (Integer, Integer) -> Bool
esOrigen' (0, 0) = True
esOrigen' (x, y) = False

-- patrón subrayado: _
esOrigen'' :: (Integer, Integer) -> Bool
esOrigen'' (0, 0) = True
esOrigen'' _      = False

-- | 6. Polimorfismo
----------------------------------------------------------------

-- versiones monomórficas (tipos concretos)

primeroI :: (Integer, Integer) -> Integer
primeroI (x, _) = x

primeroC :: (Char, Bool) -> Char
primeroC (x, _) = x

primeroB :: (Bool, Integer) -> Bool
primeroB (x, _) = x

-- versiones polimórficas (variables de tipo, genericidad en Java)

primero :: (a, b) -> a -- predefinida como fst
primero (x, _) = x

segundo :: (a, b) -> b -- predefinida como snd
segundo (_x, y) = y

-- | 7. Polimorfismo restringido o sobrecarga
----------------------------------------------------------------

-- en Java el genérico <T> puede ser reemplazado por cualquier tipo (clase)
-- en Haskell una variable de tipo a puede ser reemplazada por cualquier tipo
-- en Java puedo restringir un genérico: T extends Comparable<T>
-- en Haskell puedo restringir una variable de tipo: Ord a => a
-- una clase Haskell es semejante (pero no equivalente) a una interfaz Java

-- la clase Eq

sonSimétricos :: Eq a => (a, a) -> (a, a) -> Bool
sonSimétricos (x, y) (u, v) =
       x == v && y == u -- usamos comparación de igualdad: ==, /=

sonSimétricos' :: (Eq a, Eq b) => (a, b) -> (b, a) -> Bool
sonSimétricos' (x, y) (u, v) =
        x == v && y == u

-- la clase Ord

estáOrdenada :: Ord a => (a, a) -> Bool
estáOrdenada (x, y) = x <= y -- usamos comparación: <, <=, >, >=, compare, ==, /=

-- | 8. Definiciones locales
----------------------------------------------------------------

-- definición local, función error y pereza

raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c
   | abs a <= epsilon = error "la ecuación no es de segundo grado"
   | disc < 0 = error "raíces complejas"
   | otherwise = ((-b + raízDisc) / dosA, (-b - raízDisc) / dosA )
   where
      epsilon = 1e-8
      disc = b**2 - 4*a*c
      raízDisc = sqrt disc
      dosA = 2*a

-- | 9. Listas y patrones
----------------------------------------------------------------

--        valor                           tipo
-- [1, -2,  3 + 5,  123 `div` 6]    :: [Integer]
--
-- 1 : -2 : 3 + 5 : 123 `div` 6: [] :: [Integer]

-- número de componentes variable
-- todos los componentes deben ser del mismo tipo
-- existe la lista vacía: []
-- existe la lista unitaria: [x]
-- constructores de listas:
--      [] :: [a]
--      (:) :: a -> [a] -> [a]

-- funciones predefinidas:

{-
    head              tail
     |   ------------------------------
    [x1, x2, x3, ............, xn-1, xn]
     ------------------------------  |
                 init               last

    -------------- + -----------------
         take              drop

   null
   elem / notElem
   length
   (++)

-}

-- patrones habituales:

{-
     []               lista vacía
     [x]              lista unitaria
     (x:xs)           lista no vacía
-}

-- regla general:  [...]   ==> longitud fija,
--               (...:...) ==> longitud mínima

{-

     [x,y,z]          lista con tres elementos
     (x:y:zs)         lista con al menos dos elementos
-}

longitud :: [a] -> Integer -- predefinida como length
longitud []     =  0
longitud (_:xs) =  1 + longitud xs

ordenada :: Ord a => [a] -> Bool
ordenada []       = True
ordenada [_]      = True
ordenada (x:y:ys) =  x <= y && ordenada (y:ys)

-- | 10. Orden superior
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

-- map y secciones

-- |
-- >>> aprobadoGeneral [1..10]
-- [5.0,5.0,5.0,5.0,5.0,6.0,7.0,8.0,9.0,10.0]
--
-- >>> aprobadoGeneral [4.7, 2.5, 7, 10, 8.7]
-- [5.0,5.0,7.0,10.0,8.7]

aprobadoGeneral :: [Double] -> [Double]
aprobadoGeneral xs = map (max 5) xs

-- lambda expresiones

-- MAD = Multiply, Add, Divide

-- |
-- >> f = mad 2 3 7
-- >> f 5
-- 6
-- >> f 10
-- 2

t::(Int,Int)
t = (5,8)
xs::[Int]
xs=[1,4,6]

-- construimos una función MAD y la devolvemos como resultado
mad :: Int -> Int -> Int -> (Int -> Int)
mad m a d = \x -> ((x * m) + a) `div` d
--mad m a d  = f
--   where
--      f x = ((x * m) + a) `div` d

-- | 11. Plegado de listas
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
--suma (x:xs) = x + suma xs
suma (x:xs) = (+) x (suma xs)
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
conjunción []     = True
--conjunción (x:xs) = x && conjunción xs
conjunción (x:xs) = (&&) x (conjunción xs)
-- |
-- >>> esPalabra "haskell"
-- True
--
-- >>> esPalabra "haskell 2021"
-- False
--
-- >>> esPalabra "h"
-- True
--
-- >>> esPalabra ""
-- True

esPalabra :: String -> Bool
esPalabra []     = True
--esPalabra (x:xs) = isLetter x && esPalabra xs
esPalabra (x:xs) = f x (esPalabra xs)
   where
      f y ys = isLetter x && ys
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
todasMayúsculas []     = True
--todasMayúsculas (x:xs) = isUpper x && todasMayúsculas xs
todasMayúsculas (x:xs) = f x (todasMayúsculas xs)
   where
      f y ys = isUpper x && ys

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
máximo [x]    = x
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
mínimoYmáximo (x:xs) = (min x u, max x v)
  where (u,v) = mínimoYmáximo xs

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
aplana []       = []
--aplana (xs:xss) = xs ++ aplana xss
aplana (xs:xss) = (++) xs (aplana xss)

-- El patrón es f head solCola

recLista :: (a -> b -> b) -> b -> [a] -> b -- deducir el patrón de foldr
--recLista f solBase [] = solBase
--recLista f solBase (x:xs) = f x (recLista f solBase xs)
recLista f solBase xs = plegar xs
   where
      plegar [] = solBase
      plegar (x:xs) = f x (plegar xs)

--- ...

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
--sumaR xs = foldr (+) 0 xs
sumaR = foldr (+) 0 -- Eta reducción
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
longitudR = foldr f 0
   where
      f _ solCola = 1 + solCola

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
-- >>> esPalabraR "haskell 2021"
-- False
--
-- >>> esPalabraR "h"
-- True
--
-- >>> esPalabraR ""
-- True

esPalabraR :: String -> Bool
esPalabraR = undefined

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
todasMayúsculasR  = undefined

-- |
-- >>> máximoR "hola mundo"
-- 'u'
--
-- >>> máximoR [7, -8, 56, 17, 34, 12]
-- 56
--
-- >>> máximoR [-8]
-- -8

máximoR :: Ord a => [a] -> a
máximoR [] = error "máximoR no está definido"
máximoR (x:xs) = foldr max x xs

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
mínimoYmáximoR = undefined

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
mapR  f = foldr (\x xs -> f x : xs) []

-- |
-- >>> filterR even [1..20]
-- [2,4,6,8,10,12,14,16,18,20]
--
-- >>> filterR undefined []
-- []
--
-- >>> filterR even [5]
-- []

filterR :: (a -> Bool) -> [a] -> [a]
filterR p = foldr (\x xs -> if p x then x : xs else xs) []

-- |
-- >>> apariciones 'a' "casa"
-- 2
-- >>> apariciones 'u' "casa"
-- 0

apariciones :: Eq a => a -> [a] -> Integer
apariciones = undefined
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
purgar = undefined

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
agrupa = undefined

-- | 12. Tipos algebraicos: recursión y plegados
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
treeSize Empty = 0
treeSize (Leaf x) = (\_ -> 1) x
treeSize (Node x l r) = f x (treeSize l) (treeSize r)
   where
      f _ si sd = 1 + si + sd
-- |
-- >>> treeHeight treeI
-- 3
-- >>> treeHeight treeC
-- 4

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Leaf x) = (const 1) x
treeHeight (Node x l r) = f x  (treeHeight l)  (treeHeight r)
   where
      f _ si sd = 1 + max si sd

-- |
-- >>> treeSum treeI
-- 21

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Leaf x) = id x
treeSum (Node x l r) = f x (treeSum l) (treeSum r)
   where
      f n si sd = n + si + sd

-- |
-- >>> treeProduct treeI
-- 720

treeProduct :: Num a => Tree a -> a
treeProduct Empty = 1
treeProduct (Leaf x) = id x
treeProduct (Node x l r) = f x (treeProduct l) (treeProduct r )
   where
      f n si sd = n * si * sd
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
treeElem a Empty = False
treeElem a (Leaf x) = (== a) x
treeElem a (Node x l r) = f x (treeElem a l) (treeElem a r)
   where
      f n si sd = n == a || si || sd
-- |
-- >>> treeToList treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList treeC
-- "satgzhwfpn"

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf a) = (:[]) a
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r

-- |
-- >>> treeBorder treeI
-- [4,5,6]
--
-- >>> treeBorder treeC
-- "aghfn"

treeBorder :: Tree a -> [a]
treeBorder Empty = []
treeBorder (Leaf a) = (:[]) a
treeBorder (Node x l r) = (++) (treeBorder l) (treeBorder r)

-- introducir el plegado del tipo Tree a
plegar :: (a -> b -> b) -> b -> [a] -> b
plegar añadir solCasoBase xs = reclista xs
   where
      reclista [] = solCasoBase
      recLista (x:xs) = añadir x (recLista xs)

foldTree :: (a -> b -> b -> b) -> (a -> b) -> b -> Tree a -> b
foldTree funNode funLeaf solEmpty = recTree
   where
      recTree Empty = solEmpty
      recTree (Leaf x) = funLeaf x
      recTree (Node x l r) = funNode x (recTree l) (recTree r)

-- resolver los ejercicios anteriores con foldTree

-- |
-- >>> treeSize' treeI
-- 6
--
-- >>> treeSize' treeC
-- 10

treeSize' :: Tree a -> Integer
treeSize' = foldTree (\n si sd -> 1 + si + sd) (const 1) 0

-- |
-- >>> treeHeight' treeI
-- 3
-- >>> treeHeight' treeC
-- 4

treeHeight' :: Tree a -> Integer
treeHeight' = foldTree (\ n si sd -> 1 + max si sd) (const 1) 0

-- |
-- >>> treeSum' treeI
-- 21

treeSum' :: Num a => Tree a -> a
treeSum' = foldTree (\ n si sd -> n + si + sd) id 0

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
treeElem' x = foldTree (\ n si sd -> n == x || si || sd) (==x) False

-- |
-- >>> treeToList' treeI
-- [4,2,5,1,3,6]
--
-- >>> treeToList' treeC
-- "satgzhwfpn"

treeToList' :: Tree a -> [a]
treeToList' = foldTree (\ n si sd -> si ++ n : sd) (:[]) []

-- |
-- >>> treeBorder' treeI
-- [4,5,6]
--
-- >>> treeBorder' treeC
-- "aghfn"

treeBorder' :: Tree a -> [a]
treeBorder' = foldTree (\ n si sd -> si ++ sd) (:[]) []
