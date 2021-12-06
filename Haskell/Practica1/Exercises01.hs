{-|

Programming Languages
Fall 2021

Implementation in Haskell of the concepts covered in Chapter 1 of
Nielson & Nielson, Semantics with Applications

Author: Manuel González González

-}

module Exercises01 where

import           Test.HUnit hiding (State)
import           While
import GHC.CmmToAsm.PPC.Instr (Instr(XORIS, XOR))
import GHC.Core.Opt.Monad (bindsOnlyPass)
import Data.List ( nub )
import GHC.Plugins (bindsOnlyPass)
import GHC.BaseDir (getBaseDir)

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Bin' for the binary numerals:

data Bit = O
         | I
         deriving (Eq, Show)

data Bin = MSB Bit
         | B Bin Bit
         deriving (Eq, Show)

-- | and the following values of type 'Bin':

zero :: Bin
zero = MSB O

one :: Bin
one = MSB I

three :: Bin
three = B (B (MSB O) I) I

six :: Bin
six = B (B (MSB I) I) O

-- | define a semantic function 'binVal' that associates
-- | a number (in the decimal system) to each binary numeral.

bitVal :: Bit -> Z
bitVal O = 0
bitVal I = 1

binVal :: Bin -> Z
binVal (MSB b) = bitVal b
binVal (B bin b) = 2 * binVal bin + bitVal b
-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'


foldBin :: (Bit -> t) -> (Bit -> t -> t) -> Bin -> t
foldBin fbase frecursivo = recBin
    where
        recBin (MSB x) = fbase x
        recBin (B bin x) = frecursivo x (recBin bin)

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' = foldBin base (\x bin -> 2 * bin + base x)
    where
        base = bitVal

-- | Test your function with HUnit.
testBinVal' :: Test
testBinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"   ~: 1 ~=? binVal' one,
                   "value of three" ~: 3 ~=? binVal' three,
                   "value of six"   ~: 6 ~=? binVal' six]
-- todo

-- | Define a function 'hammingWeight' that returns the number of ones occurring
-- | in a binary numeral.

hammingWeight :: Bin -> Integer
hammingWeight (MSB O) = 0
hammingWeight (MSB I) = 1
hammingWeight (B bin O) = hammingWeight bin
hammingWeight (B bin I) = 1 + hammingWeight bin

-- | and use 'foldBin' to define a function 'hammingWeight''  equivalent to 'hammingWeight'.

hammingWeight' :: Bin -> Integer
hammingWeight' = foldBin base (\x bin -> bin + base x)
    where
        base = bitVal

-- | Test your functions with HUnit.
testHammingWeight :: Test
testHammingWeight = test ["value of zero"  ~: 0 ~=? hammingWeight zero,
                   "value of one"   ~: 1 ~=? hammingWeight one,
                   "value of three" ~: 2 ~=? hammingWeight three,
                   "value of six"   ~: 2 ~=? hammingWeight six]
testHammingWeight' :: Test
testHammingWeight' = test ["value of zero"  ~: 0 ~=? hammingWeight' zero,
                   "value of one"   ~: 1 ~=? hammingWeight' one,
                   "value of three" ~: 2 ~=? hammingWeight' three,
                   "value of six"   ~: 2 ~=? hammingWeight' six]
-- todo

-- | Define a function 'complement' that returns the complement of a binary numeral

complement :: Bin -> Bin
complement (MSB O) = MSB I
complement (MSB I) = MSB O
complement (B bin O) = B (complement bin) I
complement (B bin I) = B (complement bin) O

-- | and use 'foldBin' to define a function 'complement''  equivalent to 'complement'.

complement' :: Bin -> Bin
complement' = foldBin base recursiva
    where
        base I = MSB O
        base O = MSB I
        recursiva I bin = B bin O
        recursiva O bin = B bin I

-- | Test your functions with HUnit.

-- todo
testComplement :: Test
testComplement = test [
                    "cero" ~: MSB I ~=? complement zero,
                    "uno" ~: MSB O ~=? complement one,
                    "tres" ~: B (B (MSB I) O) O ~=? complement three,
                    "seis" ~: B (B (MSB O) O) I ~=? complement six]
testComplement' :: Test
testComplement' = test [
                    "cero" ~: MSB I ~=? complement' zero,
                    "uno" ~: MSB O ~=? complement' one,
                    "tres" ~: B (B (MSB I) O) O ~=? complement' three,
                    "seis" ~: B (B (MSB O) O) I ~=? complement' six]
-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.

normalize :: Bin -> Bin
normalize (MSB x) = MSB x
normalize (B bin x)
            | normalize bin == MSB O = MSB x
            | otherwise = B (normalize bin) x
{-
normalize (B bin bit) = putBit (normalize bin) bit

putBit :: Bin -> Bit -> Bin
putBit (MSB O) x = MSB x
putBit x b = B x b
-}

-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = foldBin MSB  (\x bin -> if bin == MSB O then MSB x else B bin x)


-- | Test your functions with HUnit.

-- todo
testNormalize :: Test
testNormalize = test [
                    "cero" ~: MSB O ~=? normalize zero,
                    "uno" ~: MSB I ~=? normalize one,
                    "tres" ~: B (MSB I) I ~=? normalize three,
                    "seis" ~: B (B (MSB I) I) O ~=? normalize six,
                    "uno v2" ~: MSB I ~=? normalize (B (B (B (MSB O) O) O) I),
                    "cero v2" ~: MSB O ~=? normalize (B (B (B (MSB O) O) O) O)]
testNormalize' :: Test
testNormalize' = test [
                    "cero" ~: MSB O ~=? normalize' zero,
                    "uno" ~: MSB I ~=? normalize' one,
                    "tres" ~: B (MSB I) I ~=? normalize' three,
                    "seis" ~: B (B (MSB I) I) O ~=? normalize' six,
                    "uno v2" ~: MSB I ~=? normalize' (B (B (B (MSB O) O) O) I),
                    "cero v2" ~: MSB O ~=? normalize' (B (B (B (MSB O) O) O) O)]

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | Define the function 'fvAexp' that computes the set of free variables
-- | occurring in an arithmetic expression. Ensure that each free variable
-- | occurs once in the resulting list.

fvAexp :: Aexp -> [Var]
fvAexp (N _) = []
fvAexp (V x) = [x]
fvAexp (Add x y) = nub (fvAexp x ++ fvAexp y) 
fvAexp (Mult x y) = nub (fvAexp x ++ fvAexp y)
fvAexp (Sub x y) = nub (fvAexp x ++ fvAexp y)

-- | Test your function with HUnit.

-- todo
testFvAexp :: Test
testFvAexp = test [
                "numeral" ~: [] ~=? fvAexp (N 7),
                "variable suelta" ~: ["x"] ~=? fvAexp (V "x"),
                "Suma" ~: ["x"] ~=? fvAexp (Add (V "x") (N 8)),
                "Producto" ~: ["y","z"] ~=? fvAexp (Mult (V "y") (V "z")),
                "Resta" ~: [] ~=? fvAexp (Sub (N 7) (N 8))]
-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp (Eq x y) = nub (fvAexp x ++ fvAexp y)
fvBexp (Le x y) = nub (fvAexp x ++ fvAexp y)
fvBexp (Neg b) = fvBexp b
fvBexp (And b b') = nub (fvBexp b ++ fvBexp b')
fvBexp _ = []


-- | Test your function with HUnit.

-- todo
testFvBexp :: Test
testFvBexp = test [
                "Negación" ~: [] ~=? fvBexp (Neg TRUE),
                "Menor que" ~: ["a"] ~=? fvBexp (Le (V "a") (N 7)),
                "Igual" ~: ["x","y"] ~=? fvBexp (Eq (V "x") (V "y")),
                "And" ~: [] ~=? fvBexp (And FALSE TRUE),
                "Caso anidado" ~: ["x","y","z"] ~=? fvBexp (And (Eq (V "x") (N 7)) (Le (V "y") (V "z")))]
-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Subst' for representing substitutions:

data Subst = Var :->: Aexp

-- | define a function 'substAexp' that takes an arithmetic expression
-- | 'a' and a substitution 'y:->:a0' and returns the substitution a [y:->:a0];
-- | i.e., replaces every occurrence of 'y' in 'a' by 'a0'.

substAexp :: Aexp -> Subst -> Aexp
substAexp (N n) _ = N n
substAexp (V x) (y :->: a) = if x == y then a else V x
substAexp (Add a1 a2) s = Add (substAexp a1 s) (substAexp a2 s)
substAexp (Mult a1 a2) s = Mult (substAexp a1 s) (substAexp a2 s)
substAexp (Sub a1 a2) s = Sub (substAexp a1 s) (substAexp a2 s)

-- | Test your function with HUnit.

-- todo
testSubstAexp :: Test
testSubstAexp = test [
            "Numeral" ~: N 7 ~=? substAexp (N 7) ("y" :->: V "z"),
            "Varible cambia" ~: V "z" ~=? substAexp (V "x") ("x" :->: V "z"),
            "Variable no cambia" ~: V "x" ~=? substAexp (V "x") ("y" :->: V "z"),
            "Suma" ~: Add (V "z") (N 7) ~=? substAexp (Add (V "x") (N 7)) ("x" :->: V "z"),
            "Producto" ~: Mult (Add (V "x") (N 7)) (V "z") ~=? substAexp (Mult (V "y") (V "z")) ("y" :->: Add (V "x") (N 7)),
            "Resta" ~: Sub (Sub (N 8) (V "a")) (V "x") ~=? substAexp (Sub (V "z") (V "x")) ("z" :->: Sub (N 8) (V "a"))]

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp FALSE _ = FALSE
substBexp TRUE _ = TRUE
substBexp (Eq a1 a2) x = Eq (substAexp a1 x) (substAexp a2 x)
substBexp (Le a1 a2) x = Le (substAexp a1 x) (substAexp a2 x)
substBexp (Neg b) x = Neg (substBexp b x)
substBexp (And b1 b2) x = And (substBexp b1 x) (substBexp b2 x)


-- | Test your function with HUnit.

-- todo
testSubstBexp :: Test
testSubstBexp = test [
            "Base Falso" ~: FALSE ~=? substBexp FALSE ("y" :->: N 5),
            "Base Verdadero" ~: TRUE ~=? substBexp TRUE ("y" :->: N 5),
            "Igual" ~: Eq (V "a") (N 7) ~=? substBexp (Eq (V "x") (N 7)) ("x" :->: V "a"),
            "Menor que" ~: Le (V "x") (Add (V "z") (N 8)) ~=? substBexp (Le (V "x") (V "y")) ("y" :->: Add (V "z") (N 8)),
            "Negación" ~: Neg (Eq (V "p") (V "y")) ~=? substBexp (Neg (Eq (V "x") (V "y"))) ("x" :->: V "p"),
            "And" ~: And TRUE (Le (N 8) (V "z")) ~=? substBexp (And TRUE (Le (V "x") (V "z"))) ("x" :->: N 8)]
-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (x :=>: v) = \k -> if k==x then v else s k

-- | Test your function with HUnit.

-- todo
testUpdate :: Test
testUpdate = test [
                "cambio" ~: 77 ~=? aVal (V "x") (update sInit ("x" :=>: 77)),
                "sin cambio" ~: aVal (V "x") sInit ~=? aVal (V "x") (update sInit ("y" :=>: 88)),
                "cambio anidado" ~: 10 ~=? aVal (Mult (V "x") (V "y")) (update (update sInit ("x" :=>: 2)) ("y" :=>: 5))]
-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates = foldl update

--updates s us = foldl update s us

--updates s [] = s
--updates s (u:us) = updates (update s u) us

testUpdates :: Test
testUpdates = test[
                "actualización" ~: 14 ~=? aVal (Mult (V "x") (V "y")) (updates sInit ["x" :=>: 2, "y" :=>: 7]),
                "valor inicial" ~: aVal (V "x") sInit ~=? aVal (V "x") (updates sInit ["x" :=>: 88, "x" :=>: 666, "x" :=>: 3])]

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (Z -> a) -> (Var -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Aexp -> a
foldAexp fn fv fa fm fs = recAexp
    where
        recAexp (N n) = fn n
        recAexp (V x) = fv x
        recAexp (Add a1 a2) = fa (recAexp a1) (recAexp a2)
        recAexp (Mult a1 a2) = fm (recAexp a1) (recAexp a2)
        recAexp (Sub a1 a2) = fs (recAexp a1) (recAexp a2)

-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''
-- | and test your definitions with HUnit.

aVal' :: Aexp -> State -> Z
aVal' a s = foldAexp id s (+) (*) (-) a

testAVal' :: Test
testAVal' = test [
                "numeral" ~: 7 ~=? aVal' (N 7) sInit,
                "variable" ~: 3 ~=? aVal' (V "x") sInit,
                "suma" ~: 10 ~=? aVal' (Add (N 7) (V "x")) sInit,
                "producto" ~: 20 ~=? aVal' (Mult (Add (N 7) (V "x")) (V "y")) (update sInit ("y" :=>: 2)),
                "resta" ~: 42 ~=? aVal' (Sub (N 45) (V "x")) sInit]

fvAexp' :: Aexp -> [Var]
fvAexp' = foldAexp (const []) (: []) f f f
    where
        f fa1 fa2 = nub (fa1 ++ fa2)

testFvAexp' :: Test
testFvAexp' = test [
                "numeral" ~: [] ~=? fvAexp' (N 7),
                "variable suelta" ~: ["x"] ~=? fvAexp' (V "x"),
                "Suma" ~: ["x"] ~=? fvAexp' (Add (V "x") (V "x")),
                "Producto" ~: ["y","z"] ~=? fvAexp' (Mult (V "y") (V "z")),
                "Resta" ~: [] ~=? fvAexp' (Sub (N 7) (N 8))]

substAexp' :: Aexp -> Subst -> Aexp
substAexp'  a (x :->: a0) = foldAexp N (\k -> if k==x then a0 else V k) Add Mult Sub a

testSubstAexp' :: Test
testSubstAexp' = test [
            "Numeral" ~: N 7 ~=? substAexp' (N 7) ("y" :->: V "z"),
            "Varible cambia" ~: V "z" ~=? substAexp' (V "x") ("x" :->: V "z"),
            "Variable no cambia" ~: V "x" ~=? substAexp' (V "x") ("y" :->: V "z"),
            "Suma" ~: Add (V "z") (N 7) ~=? substAexp' (Add (V "x") (N 7)) ("x" :->: V "z"),
            "Producto" ~: Mult (Add (V "x") (N 7)) (V "z") ~=? substAexp' (Mult (V "y") (V "z")) ("y" :->: Add (V "x") (N 7)),
            "Resta" ~: Sub (Sub (N 8) (V "a")) (V "x") ~=? substAexp' (Sub (V "z") (V "x")) ("z" :->: Sub (N 8) (V "a"))]



-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''. Test
-- | your definitions with HUnit.

foldBexp :: a -> a -> (b -> b -> a) -> (b -> b -> a) -> (a -> a) -> (a -> a -> a) -> (Aexp -> b) -> Bexp -> a
foldBexp tt ff feq fle fneg fand faexp = recBexp
    where
        recBexp TRUE = tt
        recBexp FALSE = ff
        recBexp (Eq a1 a2) = feq (faexp a1) (faexp a2)
        recBexp (Le a1 a2) = fle (faexp a1) (faexp a2)
        recBexp (Neg b) = fneg (recBexp b)
        recBexp (And b1 b2) = fand (recBexp b1) (recBexp b2)

bVal' :: Bexp -> State -> Bool
bVal' b s = foldBexp True False (==) (<=) not (&&) (\x -> aVal' x s) b

testBVal' :: Test 
testBVal' = test [
                "caso base" ~: True ~=? bVal' TRUE sInit,
                "caso Eq" ~: False ~=? bVal' (Eq (N 7) (V "x")) sInit,
                "caso Le" ~: True ~=? bVal' (Le (V "x") (N 8)) sInit,
                "caso Neg" ~: False ~=? bVal' (Neg TRUE) sInit,
                "caso And" ~: True ~=? bVal' (And TRUE (Le (V "x") (N 77))) sInit]

fvBexp' :: Bexp -> [Var]
fvBexp' = foldBexp [] [] (\x y -> nub (x++y)) (\x y -> nub (x++y)) id (\x y -> nub (x++y)) fvAexp'

testFvBexp' :: Test 
testFvBexp' = test [
                "caso base" ~: [] ~=? fvBexp' TRUE,
                "caso Eq" ~: ["x"] ~=? fvBexp' (Eq (N 7) (V "x")),
                "caso Le" ~: ["y","x"] ~=? fvBexp' (Le (V "y") (V "x")),
                "caso Neg" ~: ["x","z"] ~=? fvBexp' (Neg (Le (Add (V "x") (N 8)) (V "z"))),
                "caso And" ~: ["a","x"] ~=? fvBexp' (And (Eq (V "a") (V "x")) (Le (V "x") (N 77)))]

substBexp' :: Bexp -> Subst -> Bexp
substBexp' b s = foldBexp TRUE FALSE Eq Le Neg And (\ a -> substAexp' a s) b

testSubstBexp' :: Test
testSubstBexp' = test [
            "Base Falso" ~: FALSE ~=? substBexp' FALSE ("y" :->: N 5),
            "Base Verdadero" ~: TRUE ~=? substBexp' TRUE ("y" :->: N 5),
            "Igual" ~: Eq (V "a") (N 7) ~=? substBexp' (Eq (V "x") (N 7)) ("x" :->: V "a"),
            "Menor que" ~: Le (V "x") (Add (V "z") (N 8)) ~=? substBexp' (Le (V "x") (V "y")) ("y" :->: Add (V "z") (N 8)),
            "Negación" ~: Neg (Eq (V "p") (V "y")) ~=? substBexp' (Neg (Eq (V "x") (V "y"))) ("x" :->: V "p"),
            "And" ~: And TRUE (Le (N 8) (V "z")) ~=? substBexp' (And TRUE (Le (V "x") (V "z"))) ("x" :->: N 8)]

tests :: Test
tests = TestList [
                testBinVal,
                testBinVal',
                testHammingWeight,
                testHammingWeight',
                testComplement,
                testComplement',
                testNormalize,
                testNormalize',
                testFvAexp,
                testFvBexp,
                testSubstAexp,
                testSubstBexp,
                testUpdate,
                testUpdates,
                testAVal',
                testFvAexp',
                testSubstAexp',
                testBVal',
                testFvBexp',
                testSubstBexp']