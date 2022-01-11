{-|

Programming Languages
Fall 2020
xs
Implementation in Haskell of the concepts covered in Chapter 1 of
Nielson & Nielson, Semantics with Applications

Author: Manuel González González y Saúl García Martín

-}

module Exercises01 where

import           Test.HUnit hiding (State)
import Data.List
import While 

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

binVal :: Bin -> Z
binVal (MSB O) = 0
binVal (MSB I) = 1
binVal (B n O) = binVal n * 2
binVal (B n I) = binVal n * 2 + 1

-- | Test your function with HUnit.

testBinVal :: Test
testBinVal = test ["value of zero"  ~: 0 ~=? binVal zero,
                   "value of one"   ~: 1 ~=? binVal one,
                   "value of three" ~: 3 ~=? binVal three,
                   "value of six"   ~: 6 ~=? binVal six]

-- | Define a function 'foldBin' to fold a value of type 'Bin'

foldBin :: (Bit -> b -> b) -> (Bit -> b)-> Bin -> b
foldBin f n = recBin
  where
      recBin (MSB x) = n x
      recBin (B s x) = f x (recBin s)

-- | and use 'foldBin' to define a function 'binVal''  equivalent to 'binVal'.

binVal' :: Bin -> Integer
binVal' = foldBin f n 
  where
      f cola lista = n cola + 2 * lista
      n O = 0
      n I = 1 

-- | Test your function with HUnit.

-- todo
testBinVal' :: Test
testBinVal' = test ["value of zero"  ~: 0 ~=? binVal' zero,
                   "value of one"   ~: 1 ~=? binVal' one,
                   "value of three" ~: 3 ~=? binVal' three,
                   "value of six"   ~: 6 ~=? binVal' six]
-- | Define a function 'hammingWeight' that returns the number of ones occurring
-- | in a binary numeral.

hammingWeight :: Bin -> Integer
hammingWeight (MSB O) = 0
hammingWeight (MSB I) = 1
hammingWeight (B n O) = hammingWeight n
hammingWeight (B n I) = 1 + hammingWeight n


-- | and use 'foldBin' to define a function 'hammingWeight''  equivalent to 'hammingWeight'.

hammingWeight' :: Bin -> Integer
hammingWeight' = foldBin f n
  where
    f cola lista = n cola + lista
    n O = 0
    n I = 1

-- | Test your functions with HUnit.

-- todo
testHammingWeight :: Test
testHammingWeight = test [
  "value of zero"  ~: 0 ~=? hammingWeight zero,
  "value of one"   ~: 1 ~=? hammingWeight one,
  "value of three" ~: 2 ~=? hammingWeight three,
  "value of six"   ~: 2 ~=? hammingWeight six]

testHammingWeight' :: Test
testHammingWeight' = test [
  "value of zero"  ~: 0 ~=? hammingWeight' zero,
  "value of one"   ~: 1 ~=? hammingWeight' one,
  "value of three" ~: 2 ~=? hammingWeight' three,
  "value of six"   ~: 2 ~=? hammingWeight' six]


-- | Define a function 'complement' that returns the complement of a binary numeral

complement :: Bin -> Bin
complement (MSB O) = MSB I
complement (MSB I) = MSB O
complement (B x O) = B (complement x) I
complement (B x I) = B (complement x) O


-- | and use 'foldBin' to define a function 'complement''  equivalent to 'complement'.

complement' :: Bin -> Bin
complement' = foldBin f n
  where
    f I lista = B lista O
    f O lista = B lista I
    n O = MSB I
    n I = MSB O


-- | Test your functions with HUnit.

-- todo
testComplement :: Test
testComplement = test ["value of zero"  ~: MSB I ~=? complement zero,
                   "value of one"   ~: MSB O ~=? complement one,
                   "value of three" ~: B (B (MSB I) O) O ~=? complement three,
                   "value of six"   ~: B (B (MSB O) O) I ~=? complement six]
testComplement' :: Test
testComplement' = test ["value of zero"  ~: MSB I ~=? complement' zero,
                   "value of one"   ~: MSB O ~=? complement' one,
                   "value of three" ~: B (B (MSB I) O) O ~=? complement' three,
                   "value of six"   ~: B (B (MSB O) O) I ~=? complement' six]


-- | Define a function 'normalize' that given a binary numeral trims leading zeroes.

normalize :: Bin -> Bin
normalize (MSB O) = MSB O
normalize (MSB I) = MSB I
normalize (B x y) = if normalize x==MSB O then MSB y else B (normalize x) y


-- | and use 'foldBin' to define a function 'normalize''  equivalent to 'normalize'.

normalize' :: Bin -> Bin
normalize' = foldBin f n
  where
    f cola lista = if normalize' lista == MSB O then MSB cola else B (normalize' lista) cola
    n O = MSB O
    n I = MSB I

-- | Test your functions with HUnit.

-- todo

testNormalize :: Test
testNormalize = test ["value of zero"  ~: MSB O ~=? normalize zero,
                   "value of one"   ~: MSB I ~=? normalize one,
                   "value of three" ~: B (MSB I) I ~=? normalize three,
                   "value of six"   ~: B (B (MSB I) I) O ~=? normalize six,
                   "value of (B (B (B (MSB O) I) O) I)" ~: B (B (MSB I) O) I ~=? normalize (B (B (B (MSB O) I) O) I),
                   "value of (B (B (B (B (MSB O) O) I) O) I)" ~: B (B (MSB I) O) I ~=? normalize (B (B (B (B (MSB O) O) I) O) I)]

testNormalize' :: Test
testNormalize' = test ["value of zero"  ~: MSB O ~=? normalize' zero,
                   "value of one"   ~: MSB I ~=? normalize' one,
                   "value of three" ~: B (MSB I) I ~=? normalize' three,
                   "value of six"   ~: B (B (MSB I) I) O ~=? normalize' six,
                   "value of (B (B (B (MSB O) I) O) I)" ~: B (B (MSB I) O) I ~=? normalize' (B (B (B (MSB O) I) O) I),
                   "value of (B (B (B (B (MSB O) O) I) O) I)" ~: B (B (MSB I) O) I ~=? normalize' (B (B (B (B (MSB O) O) I) O) I)]



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

testAexp :: Test
testAexp = test["empty" ~: [] ~=? fvAexp (N 10),
              "one var" ~: ["x"] ~=? fvAexp (V "x"),
              "several of the same var" ~: ["x"] ~=? fvAexp (Mult (Add (V "x") (V "x")) (Sub (V "x") (V "x"))),
              "several vars" ~: ["x", "y", "z"] ~=? fvAexp (Add (V "x") (Sub (V "y") (V "z"))),
              "Add" ~: ["cuatro","cinco"] ~=? fvAexp (Add (V "cuatro") (V "cinco")),
              "Add (Add)" ~: ["cuatro","cinco"] ~=? fvAexp (Add (N 4) (Add (V "cuatro") (V "cinco"))),
              "Mult" ~: ["uno"] ~=? fvAexp (Mult (V "uno") (N 2)),
              "Sub" ~: ["siete"] ~=? fvAexp (Sub (V "siete") (V "siete")),
              "Add (Mult (Sub)))" ~: ["uno","cuatro","cinco"] ~=? fvAexp (Add (V "uno") (Mult (V "cuatro") (Sub (V "cinco") (N 7))))]
-- todo

-- | Define the function 'fvBexp' that computes the set of free variables
-- | occurring in a Boolean expression.

fvBexp :: Bexp -> [Var]
fvBexp TRUE = []
fvBexp FALSE = []
fvBexp (Eq e1 e2) = nub (fvAexp e1 ++ fvAexp e2)
fvBexp (Le e1 e2) = nub (fvAexp e1 ++ fvAexp e2)
fvBexp (Neg be1) = fvBexp be1
fvBexp (And be1 be2) = nub (fvBexp be1 ++ fvBexp be2)

testBexp :: Test
testBexp = test["true" ~: [] ~=? fvBexp TRUE,
              "false" ~: [] ~=? fvBexp FALSE,
              "equals" ~: ["x", "y"] ~=? fvBexp (Eq (V "x") (V "y")),
              "lesser" ~: ["x"] ~=? fvBexp (Le (V "x") (V "x")),
              "negation" ~: ["x", "y"] ~=? fvBexp (Neg (Eq (V "x") (V "y"))),
              "and" ~: ["x", "y", "z"] ~=? fvBexp (And (Neg (Eq (V "x") (V "y"))) (Le (V "x") (V "z")))]

-- | Test your function with HUnit.

-- todo

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
substAexp (V x) (y :->: a0) = if x==y then a0 else V x
substAexp (Add e1 e2) s = Add (substAexp e1 s) (substAexp e2 s)
substAexp (Mult e1 e2) s = Mult (substAexp e1 s) (substAexp e2 s)
substAexp (Sub e1 e2) s = Sub (substAexp e1 s) (substAexp e2 s)

-- | Test your function with HUnit.

testSubstAexp :: Test
testSubstAexp = test["number" ~: N 10 ~=? substAexp (N 10) ("y" :->: N 40),
                  "right var" ~: N 40 ~=? substAexp (V "y") ("y" :->: N 40),
                  "wrong var" ~: V "x" ~=? substAexp (V "x") ("y" :->: N 40),
                  "general substitution" ~: Add (N 10) (Mult (Sub (V "y") (N 5)) (V "y")) ~=? substAexp (Add (N 10) (Mult (V "x") (V "y"))) ("x" :->: Sub (V "y") (N 5)),
                  "Add" ~: Add (V "siete") (N 7) ~=? substAexp (Add (V "seis") (N 7)) ("seis" :->: V "siete"),
                  "Mult" ~: Mult (V "siete") (V "ocho") ~=? substAexp (Mult (V "seis") (V "ocho")) ("seis" :->: V "siete"),
                  "Sub" ~: Sub (N 12) (V "ocho") ~=? substAexp (Sub (N 12) (V "doce")) ("doce" :->: V "ocho"),
                  "Add (Mult (Sub))" ~: Add (N 12) (Mult (V "ocho") (Sub (N 3) (V "ocho"))) ~=? substAexp (Add (N 12) (Mult (V "doce") (Sub (N 3) (V "doce")))) ("doce" :->: V "ocho")]

-- todo

-- | Define a function 'substBexp' that implements substitution for
-- | Boolean expressions.

substBexp :: Bexp -> Subst -> Bexp
substBexp TRUE _ = TRUE
substBexp FALSE _ = FALSE
substBexp (Eq e1 e2) s = Eq (substAexp e1 s) (substAexp e2 s)
substBexp (Le e1 e2) s = Le (substAexp e1 s) (substAexp e2 s)
substBexp (Neg be1) s = Neg (substBexp be1 s)
substBexp (And be1 be2) s = And (substBexp be1 s) (substBexp be2 s)

-- | Test your function with HUnit.

testSubstBexp :: Test
testSubstBexp = test["true" ~: TRUE ~=? substBexp TRUE ("y" :->: N 40),
                  "false" ~: FALSE ~=? substBexp FALSE ("y" :->: N 40),
                  "eq" ~: Eq (V "x") (N 40) ~=? substBexp (Eq (V "x") (V "y")) ("y" :->: N 40),
                  "le" ~: Le (V "z") (V "y") ~=? substBexp (Le (V "x") (V "y")) ("x" :->: V "z"),
                  "neg" ~: Neg (Le (V "x") (Add (N 10) (N 20))) ~=? substBexp (Neg (Le (V "x") (V "y"))) ("y" :->: Add (N 10) (N 20)),
                  "Prueba 1" ~: And (Eq (V "cinco") (N 5)) (Neg (Le (V "cuatro") (V "cinco"))) ~=? substBexp (And (Eq (V "seis") (N 5)) (Neg (Le (V "cuatro") (V "seis")))) ("seis" :->: V "cinco"),
                  "Prueba 2" ~: And TRUE (Neg FALSE) ~=? substBexp (And TRUE (Neg FALSE)) ("FALSE" :->: V "TRUE")] 

-- todo

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | Given the algebraic data type 'Update' for state updates:

data Update = Var :=>: Z

-- | define a function 'update' that takes a state 's' and an update 'x :=> v'
-- | and returns the updated state 's [x :=> v]'

update :: State -> Update -> State
update s (y :=>: x) = (\k -> if k==y then x else s k)
 
-- | Test your function with HUnit.

sTest :: State
sTest "x" =  3
sTest "y"  = 6
sTest "z"  = 10
sTest _ = 0

testUpdate :: Test
testUpdate = test["cambio basico" ~: 30  ~=? aVal (V "x") (update sTest ("x" :=>: 30)),
              "sin cambio" ~: aVal (V "x") sTest ~=? aVal (V "x") (update sTest ("y" :=>: 30)),
              "cambio avanzado" ~: 35 ~=? aVal (Add (V "x") (V "y")) (update (update sTest ("x" :=>: 15)) ("y" :=>: 20))]

-- todo

-- | Define a function 'updates' that takes a state 's' and a list of updates
-- | 'us' and returns the updated states resulting from applying the updates
-- | in 'us' from head to tail. For example:
-- |
-- |    updates s ["x" :=>: 1, "y" :=>: 2, "x" :=>: 3]
-- |
-- | returns a state that binds "x" to 3 (the most recent update for "x").

updates :: State ->  [Update] -> State
updates s [] = s
updates s (u:us) = updates (update s u) us


testUpdates :: Test
testUpdates = test["cambio avanzado" ~: 35 ~=? aVal (Add (V "x") (V "y")) (updates sTest ["x" :=>: 15, "y" :=>: 20]),
                "cambiar misma variable" ~: 5 ~=? aVal (V "x") (updates sTest ["x" :=>: 20, "x" :=>: 666, "x" :=>: 5])]

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------
-- | Define a function 'foldAexp' to fold an arithmetic expression

foldAexp :: (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (Var -> b) -> (Integer -> b) -> Aexp -> b
foldAexp  fa fm fs n m = recAexp
  where
    recAexp (N a) = m a
    recAexp (V x) = n x
    recAexp (Add e1 e2) = fa (recAexp e1) (recAexp e2)
    recAexp (Mult e1 e2) = fm (recAexp e1) (recAexp e2)
    recAexp (Sub e1 e2) = fs (recAexp e1) (recAexp e2) 

-- | Use 'foldAexp' to define the functions 'aVal'', 'fvAexp'', and 'substAexp''
-- | and test your definitions with HUnit.

aVal' :: Aexp -> State -> Z
aVal' exp s = foldAexp fa fm fs n m exp
  where
    m a = a
    n x = s x
    fa e1 e2 = (+) e1 e2
    fm e1 e2 = (*) e1 e2
    fs e1 e2 = (-) e1 e2

fvAexp' :: Aexp -> [Var]
fvAexp' = foldAexp fa fm fs n m
  where
    m _ = []
    n x = [x]
    fa e1 e2 = nub (e1 ++ e2)
    fm e1 e2 = nub (e1 ++ e2)
    fs e1 e2 = nub (e1 ++ e2)


substAexp' :: Aexp -> Subst -> Aexp
substAexp' exp (y :->: a0) = foldAexp fa fm fs n m exp
  where
    m n = N n
    n x = if x==y then a0 else V x
    fa e1 e2 = Add e1 e2
    fm e1 e2 = Mult e1 e2
    fs e1 e2 = Sub e1 e2


testFoldAexp :: Test
testFoldAexp = test["aVal" ~: 9 ~=? aVal' (Add (V "x") (V "y")) sTest,
                "fvAexp" ~: ["x", "y", "z"] ~=? fvAexp' (Add (V "x") (Sub (V "y") (V "z"))),
                "substAexp" ~: Add (N 10) (Mult (Sub (V "y") (N 5)) (V "y")) ~=? substAexp' (Add (N 10) (Mult (V "x") (V "y"))) ("x" :->: Sub (V "y") (N 5))]

-- | Define a function 'foldBexp' to fold a Boolean expression and use it
-- | to define the functions 'bVal'', 'fvBexp'', and 'substAexp''. Test
-- | your definitions with HUnit.

foldBexp :: (Aexp -> a) -> (a -> a -> b) -> (a -> a -> b) -> (b -> b) -> (b -> b -> b) -> b -> b -> Bexp -> b
foldBexp fAexp feq fle fneg fand t f = recBexp
  where
    recBexp TRUE = t
    recBexp FALSE = f
    recBexp (Eq ex1 ex2) = feq (fAexp ex1) (fAexp ex2)
    recBexp (Le ex1 ex2) = fle (fAexp ex1) (fAexp ex2)
    recBexp (Neg bex) = fneg (recBexp bex)
    recBexp (And bex1 bex2) = fand (recBexp bex1) (recBexp bex2)


bVal' :: Bexp -> State -> Bool
bVal' exp s = foldBexp (fAexp s) fe fl fn fa t f exp
  where
    t = True
    f = False
    fAexp s exp = aVal' exp s
    fe e1 e2 = (==) e1 e2
    fl e1 e2 = (<=) e1 e2
    fn e = not e
    fa e1 e2 = (&&) e1 e2
     

fvBexp' :: Bexp -> [Var]
fvBexp' exp = foldBexp fvAexp' fe fl fn fa t f exp
  where
    t = []
    f = []
    fe e1 e2 = nub (e1 ++ e2)
    fl e1 e2 = nub (e1 ++ e2)
    fn e = nub (e)
    fa e1 e2 = nub (e1 ++ e2)

substBexp' :: Bexp -> Subst -> Bexp
substBexp' = undefined
