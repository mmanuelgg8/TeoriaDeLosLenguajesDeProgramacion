{-|

Programming Languages
Fall 2021

Implementation of the Natural Semantics of the While Language

Reference: Chapter 2 of Nielson & Nielson, "Semantics with Applications"

Author: Manuel González González

-}

module Exercises02 where

import           NaturalSemantics
import           Test.HUnit       hiding (State)

import           Aexp
import           Bexp
import           While
import           WhileParser
import Data.List (nub)
import           WhileExamples

{-
   Note: You need to either copy or import the data type 'Update' and
   the functions 'fvAexp', 'fvBexp', and 'update' from Lab Exercise
   for Unit 2, "Semantics of Expressions".
-}

import Exercises01

-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------
-- | The function 'sNs' returns the final state of the execution of a
-- | WHILE statement 'st' from a given initial state 's'. For example:
-- |
-- |  sNs factorial factorialInit
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
-- | "show" a state thus you can inspect the final state yielded by the
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
showState s = map (\ x -> x ++ " -> " ++ show (s x))

-- | Test your function with HUnit.

testShowState :: Test
testShowState = test [
            "x" ~: ["x -> 3"] ~=? showState sInit ["x"],
            "x y" ~: ["x -> 3", "y -> 0"] ~=? showState sInit ["x","y"]]

-- | Exercise 1.2
-- | Define a function 'fvStm' that returns the free variables of a WHILE
-- | statement. For example:
-- |
-- | fvStm factorial = ["y","x"]
-- |
-- | Note: the order of appearance is not relevant, but there should not be
-- | duplicates.


fvStm :: Stm -> [Var]
fvStm = nub . fvStmLoc
  where
    fvStmLoc (Ass x a) = x : (fvAexp a)
    fvStmLoc (Skip) = []
    fvStmLoc (Comp s1 s2) = (fvStm s1) ++ (fvStm s2)
    fvStmLoc (If b s1 s2) = (fvStm s1) ++ (fvStm s2)
    fvStmLoc (While b s) = (fvStm s)

-- | Test your function with HUnit. Beware the order of appearance.

testFvStm :: Test
testFvStm = test [
            "factorial" ~: ["y","x"] ~=? fvStm factorial,
            "swap" ~: ["z","x","y"] ~=? fvStm swap]

-- | Exercise 1.3
-- | Define a function 'showFinalState' that given a WHILE statement and
-- | an initial state returns a list of strings with the bindings of
-- | the free variables of the statement in the final state. For
-- | example:
-- |
-- |  showFinalState factorial factorialInit = ["y->6","x->1"]

showFinalState :: Stm -> State -> [String]
showFinalState st s = undefined

-- | Test your function with HUnit. Beware the order or appearance.


-- | Exercise 1.4
-- | Use the function 'run' below to execute the While programs 'Divide.w'
-- | and 'Factorial.w' in the directory 'Examples' to check your implementation
-- | of the Natural Semantics. For example:
-- |
-- |  > run "Examples/Factorial.w"
-- |
-- | Write a few more While programs. For example, write a While program to
-- | compute x^y.


-- | Run the While program stored in filename and show final values of variables
run :: String -> IO()
run filename =
  do
     (program, vars, stm) <- parser filename
     let Final s = nsStm (Inter stm (const 0))
     putStrLn $ "Program " ++ program ++ " finalized."
     putStr "Final State: "
     print $ showState s vars

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' statement.
-- | The file Examples/FactorialRepeat.w contains a simple program to
-- | compute the factorial with a 'repeat until' loop.

-- | Exercise 2.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'


        [repeat-ff]  ------------------------------   ¿condition?




        [repeat-tt]  ------------------------------   ¿condition?


-}

-- | Exercise 2.2
-- | Extend the definition of 'nsStm' in module NaturalSemantics.hs
-- | to include the 'repeat S until b' statement.


-- | Exercise 2.3
-- | Write a couple of WHILE programs that use the 'repeat' statement and
-- | test your functions with HUnit.


-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'for x:= a1 to a2 do S'
-- | statement.
-- | The file Examples/FactorialFor.w contains a simple program to compute
-- | the factorial with a 'for' loop.
-- | The file Examples/ForTests.w contains a more contrived example illustrating
-- | some subtle points of the semantics of the for loop.

-- | Exercise 3.1
-- | Define the natural semantics of this new statement. You are not allowed
-- | to rely on the 'while b do S' or the 'repeat S until b' statements.

{- Formal definition of 'for x:= a1 to a2 do S'


-}

-- | Exercise 3.2
-- | Extend  the definition 'nsStm' in  module NaturalSemantics.hs
-- | to include the 'for x:= a1 to a2 do S' statement.

-- | Exercise 3.3
-- | Write a couple of  WHILE programs that use the 'for' statement
-- | and test your functions with HUnit.

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------

-- | Define the semantics of arithmetic expressions (Aexp) by means of
-- | natural semantics. To that end, define an algebraic datatype 'ConfigAexp'
-- | to represent the configurations, and a function 'nsAexp' to represent
-- | the transition relation.

-- representation of configurations for Aexp, (replace TODO by appropriate
-- data definition)

data ConfigAExp = TODO

-- representation of the transition relation <A, s> -> z

nsAexp :: ConfigAExp -> ConfigAExp
nsAexp = undefined

-- | Test your function with HUnit. Inspect the final states of at least
-- | four different evaluations.

-- |----------------------------------------------------------------------
-- | Exercise 5
-- |----------------------------------------------------------------------

-- | In the statement 'for x:= a1 to a2 S' the variable 'x' is the control
-- | variable. Some programming languages protect this variable in that
-- | it cannot be assigned to in the body of the loop, S.
-- | For example, the program below:
-- |
-- |    y := 1;
-- |    for x:= 1 to 10 do begin
-- |       y := y * x;
-- |       x := x + 1    // assignment to control variable
-- |    end
-- |
-- | would be rejected by languages enforcing such a restriction.
-- | Note that this check is performed before the program is executed,
-- | and therefore is a static semantics check.

-- | Exercise 5.1
-- | Using axioms and inference rules, define a deduction system that
-- | checks whether a WHILE program satisfies this restriction.
{-
    C |- Stm -> Bool
    C |- skip -> True
    C |- x := a -> x not in C

            C U {x} |- S -> V
    -------------------------------------
      C |- for x := a1 to a2 do S -> V

      C |- S1 -> V1   C |- S2 -> V2
    --------------------------------
        C |- S1; S2 -> V1 and V2

      C |- S1 -> V1        C |- S2 -> V2
    --------------------------------------
    C |- if b then S1 else S2 -> V1 and V2 

          C |- S -> V
    -----------------------------
       C |- while b do S -> V

           C |- S -> V
    ----------------------------
      C |- repeat S until b -> V   

-}
-- | Exercise 5.2
-- | Define a function 'forLoopVariableCheck :: Stm -> Bool' that implements
-- | the static semantics check above described.

forLoopVariableCheck :: Stm -> Bool
forLoopVariableCheck = undefined

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
nsDeriv stm s = undefined

-- | Convert concrete syntax to abstract syntax

concreteToAbstract :: String -> String -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, _, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s
