{-|

Programming Languages
Fall 2020

Implementation in Haskell of the Natural Semantics described in Chapter 2 of
Nielson & Nielson, Semantics with Applications

Authors: Saúl García Martín y Manuel González González
-}

module Exercises02 where

import           Exercises01      (Update (..), fvAexp, fvBexp, update)
import           NaturalSemantics
import           Test.HUnit       hiding (State)
import           While
import           WhileParser
import           WhileLexer
import           Data.List
-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | The function 'sNs' returns the final state of the execution of a
-- | WHILE statement 'st' from a given initial state 's'. For example:
-- |
-- |  sNs factorial sInit
-- |
-- | returns the final state:
-- |
-- |    s x = 1
-- |    s y = 6
-- |    s _ = 0
-- |
-- | Since a state is a function it cannot be printed thus you cannot
-- | add 'deriving Show' to the algebraic data type 'Config'.
-- | The goal of this exercise is to define a number of functions to
-- | "show" a state thus you can inspect the final state computed by the
-- | natural semantics of WHILE.

-- | Exercise 1.1
-- | Define a function 'showState' that given a state 's' and a list
-- | of variables 'vs' returns a list of strings showing the bindings
-- | of the variables mentioned in 'vs'. For example, for the state
-- | 's' above we get:
-- |
-- |    showState s ["x"] = ["x -> 1"]
-- |    showState s ["y"] = ["y -> 6"]
-- |    showState s ["x", "y"] = ["x -> 1", "y -> 6"]
-- |    showState s ["y", "z", "x"] = ["y -> 6", "z -> 0", "x -> 1"]

showState :: State -> [Var] -> [String]
showState s [] = []
showState s (x:xs) =  [x ++ " -> " ++ show (aVal (V x) s)] ++ showState s xs

-- | Test your function with HUnit.

sttest :: State
sttest "x" = 1
sttest "z" = 7
sttest _ = 0


testShowState :: Test
testShowState = test [  "[x]" ~: ["x -> 1"] ~=? showState sttest ["x"],
                        "[z]" ~: ["z -> 7"] ~=? showState sttest ["z"],
                        "[x,z]" ~: ["x -> 1", "z -> 7"] ~=? showState sttest ["x", "z"],
                        "[z,y,x]" ~: ["z -> 7", "y -> 0", "x -> 1"] ~=? showState sttest ["z", "y", "x"]]

-- | Exercise 1.2
-- | Define a function 'fvStm' that returns the free variables of a WHILE
-- | statement. For example:
-- |
-- | fvStm factorial = ["y","x"]
-- |
-- | Note: the order of appearance is not relevant, but there should not be
-- | duplicates.

fvStm :: Stm -> [Var]
fvStm (Ass x a) = nub (fvAexp (V x) ++ fvAexp a)
fvStm Skip = [] 
fvStm (Comp s1 s2) = nub (fvStm s1 ++ fvStm s2)
fvStm (If b s1 s2) = nub (fvBexp b ++ fvStm s1 ++ fvStm s2)
fvStm (While b s) = nub (fvBexp b ++ fvStm s)

-- | Test your function with HUnit. Beware the order or appearance.

testFvStm :: Test
testFvStm = test [  "Fv Factorial" ~: ["y", "x"] ~=? fvStm factorial,
                    "Fv Power" ~: ["z", "y", "x"] ~=? fvStm power]

-- | Exercise 1.3
-- | Define a function 'showFinalState' that given a WHILE statement and a
-- | initial state returns a list of strings with the bindings of
-- | the free variables of the statement in the final state. For
-- | example:
-- |
-- |  showFinalState factorial sInit = ["y->6","x->1"]

showFinalState :: Stm -> State -> [String]
showFinalState st s = showState (sNs st s) (fvStm st)

-- | Test your function with HUnit. Beware the order or appearance.

testShowFinalState :: Test
testShowFinalState = test [ "factorial sInit" ~: ["y -> 6", "x-> 1"] ~=? showFinalState factorial sInit]

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | Write a program in WHILE to compute z = x^y and check it by obtaining a
-- | number of final states.

power :: Stm -- WHILE statement to compute z = x^y
power = Comp (Ass "z" (N 1)) 
        (While (Le (N 1) (V "y")) 
              (Comp (Ass "z" (Mult (V "z") (V "x"))) 
                    (Ass "y" (Sub (V "y") (N 1)))))

-- | Test your function with HUnit. Inspect the final states of at least
-- | four different executions.

sPower1 :: State
sPower1 "x" = 1
sPower1 "y" = 2
sPower1 _ = 0

sPower2 :: State
sPower2 "x" = 2
sPower2 "y" = 2
sPower2 _ = 0

sPower3 :: State
sPower3 "x" = 3
sPower3 "y" = 4
sPower3 _ = 0

sPower4 :: State
sPower4 "x" = 2
sPower4 "y" = 6
sPower4 _ = 0

sPower5 :: State
sPower5 "x" = 2
sPower5 "y" = 4
sPower5 _ = 0


testPower :: Test
testPower = test [  "1^2" ~: ["z -> 1", "y -> 0", "x -> 1"] ~=? showFinalState power sPower1,
                    "2^2" ~: ["z -> 4", "y -> 0", "x -> 2"] ~=?  showFinalState power sPower2,
                    "3^4" ~: ["z -> 81", "y -> 0", "x -> 3"] ~=?  showFinalState power sPower3,
                    "2^6" ~: ["z -> 64", "y -> 0", "x -> 2"] ~=?  showFinalState power sPower4,
                    "2^4" ~: ["z -> 16", "y -> 0", "x -> 2"] ~=?  showFinalState power sPower5]

-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' construct.

-- | Exercise 3.1
-- | Define the natural semantics of this new construct. You are not allowed
-- | to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'


        [repeat-ff]  ------------------------------   ¿condition?




        [repeat-tt]  ------------------------------   ¿condition?


-}

-- | Extend  the definitions of  data type 'Stm' (in  module While.hs)
-- |  and  'nsStm'  (in  module NaturalSemantics.hs)  to  include  the
-- | 'repeat  S until b' construct.  Write a couple of  WHILE programs
-- | that use the 'repeat' statement and test your functions with HUnit.

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'for x:= a1 to a2 do S'
-- | construct.

-- | Exercise 4.1
-- | Define the natural semantics of this new construct. You are not allowed
-- | to rely on the 'while b do S' or the 'repeat S until b' statements.

{- Formal definition of 'for x:= a1 to a2 do S'


-}

-- | Extend  the definitions of  data type 'Stm' (in  module While.hs)
-- | and  'nsStm'  (in  module NaturalSemantics.hs)  to  include  the
-- | 'for x:= a1 to a2 do S' construct.  Write a couple of  WHILE programs
-- | that use the 'for' statement and test your functions with HUnit.


-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | Define the semantics of arithmetic expressions (Aexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigAexp'
-- | to represent the configurations, and a function 'nsAexp' to represent
-- | the transition relation.

-- representation of configurations for Aexp, (replace TODO by appropriate
-- data definition)

data ConfigAExp = InterA Aexp State -- <A, s>
                | FinalA Z      -- z

-- representation of the transition relation <A, s> -> z

nsAexp :: ConfigAExp -> ConfigAExp

-- n 
nsAexp (InterA (N z) s) = FinalA z
-- v
nsAexp (InterA (V v) s) = FinalA (aVal (V v) s)
-- a1 + a2
nsAexp (InterA (Add a1 a2) s) = FinalA (z1 + z2)
  where FinalA z1 = nsAexp (InterA a1 s)
        FinalA z2 = nsAexp (InterA a2 s)
-- a1 * a2
nsAexp (InterA (Mult a1 a2) s) = FinalA (z1 * z2)
  where FinalA z1 = nsAexp (InterA a1 s)
        FinalA z2 = nsAexp (InterA a2 s)
-- a1 - a2
nsAexp (InterA (Sub a1 a2) s) = FinalA (z1 - z2)
  where FinalA z1 = nsAexp (InterA a1 s)
        FinalA z2 = nsAexp (InterA a2 s)

showFinalZ :: ConfigAExp -> Z
showFinalZ (InterA a s) = zf
   where FinalA zf = nsAexp (InterA a s)
showFinalZ (FinalA z) = z

-- showFinalZ (nsAexp (InterA (N 10) sInit))
-- showFinalZ (nsAexp (InterA (V "x") sInit))

-- | Test your function with HUnit. Inspect the final states of at least
-- | four different evaluations.

testNsAexp :: Test
testNsAexp = test [ "1" ~: 10 ~=? showFinalZ (nsAexp (InterA (N 10) sInit)),
                    "2" ~: 3 ~=? showFinalZ (nsAexp (InterA (V "x") sInit)),
                    "3" ~: 9 ~=? showFinalZ (nsAexp (InterA (Mult (N 3) (V "x")) sInit)),
                    "4" ~: 12 ~=? showFinalZ (nsAexp (InterA (Mult (Sub (V "x") (V "y")) (V "z")) sTesting)),
                    "5" ~: 20 ~=? showFinalZ (nsAexp (InterA (Add (Mult (Sub (V "x") (V "y")) (V "z")) (N 8)) sTesting))]

-- |----------------------------------------------------------------------
-- | Exercise 6
-- |----------------------------------------------------------------------

-- | Given the algebraic data type 'DerivTree' to represent derivation trees
-- | of the natural semantics:

data Transition = Config :-->: State

data DerivTree = AssNS     Transition
               | SkipNS    Transition
               | CompNS    Transition DerivTree DerivTree
               | IfTTNS    Transition DerivTree
               | IfFFNS    Transition DerivTree
               | WhileTTNS Transition DerivTree DerivTree
               | WhileFFNS Transition

-- | and the function 'getFinalState' to access the final state of the root
-- | of a derivation tree:

getFinalState :: DerivTree -> State
getFinalState (AssNS  (_ :-->: s))         = s
getFinalState (SkipNS (_ :-->: s))         = s
getFinalState (CompNS (_ :-->: s) _ _ )    = s
getFinalState (IfTTNS (_ :-->: s) _ )      = s
getFinalState (IfFFNS (_ :-->: s) _ )      = s
getFinalState (WhileTTNS (_ :-->: s) _ _ ) = s
getFinalState (WhileFFNS (_ :-->: s))      = s

-- | Define a function 'nsDeriv' that given a WHILE statement 'st' and an
-- | initial state 's' returns corresponding derivation tree.

nsDeriv :: Stm -> State -> DerivTree
nsDeriv (Ass x a) s = AssNS (Inter (Ass x a) s :-->: sf)
  where Final sf = nsStm (Inter (Ass x a) s)

nsDeriv Skip s = SkipNS (Inter Skip s :-->: s)

nsDeriv (Comp s1 s2) s = CompNS (Inter (Comp s1 s2) s :-->: sf) (nsDeriv s1 s) (nsDeriv s2 ss)
  where ss = getFinalState (nsDeriv s1 s)
        sf = getFinalState (nsDeriv s2 ss)

nsDeriv (If b s1 s2) s 
  | bVal b s = IfTTNS (Inter (If b s1 s2) s :-->: sf) (nsDeriv s1 s)
  where sf = getFinalState (nsDeriv s1 s)

nsDeriv (If b s1 s2) s 
  | bVal (Neg b) s = IfFFNS (Inter (If b s1 s2) s :-->: sf) (nsDeriv s2 s)
  where sf = getFinalState (nsDeriv s2 s)

nsDeriv (While b ss) s
  | bVal b s = WhileTTNS (Inter (While b ss) s :-->: sf) (nsDeriv ss s) (nsDeriv (While b ss) s2)
  where s2 = getFinalState (nsDeriv ss s)
        sf = getFinalState (nsDeriv (While b ss) s2)

nsDeriv (While b ss) s
  | bVal (Neg b) s = WhileFFNS (Inter (While b ss) s :-->: s)

nsDeriv (Repeat ss b) s = undefined

nsDeriv (For v a1 a2 ss) s = undefined


-- Some executions:
-- showState (getFinalState(nsDeriv factorial sInit)) ["x","y"]
-- showState (getFinalState(nsDeriv power sInit)) ["x","y","z"]

-- | Convert concrete syntax to abstract syntax

concreteToAbstract :: String -> String -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s

-- | Convert concrete syntax to String

concreteToString :: String -> String -> IO()
concreteToString inputFile programName =
  do
    xs <- readFile inputFile
    let ys = map (\ l -> "   \\ " ++ l ++ "\\n\\") (lines xs)
    putStrLn $ programName ++ " :: String"
    putStrLn $ programName ++ " = "
    putStrLn "   \"\\"
    mapM_ putStrLn (init ys)
    putStrLn $ (init .last) ys ++ "\""

-- | Run the While program stored in filename and show final values of variables

run :: String -> IO()
run filename =
  do
     (vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     print $ showState s vars
