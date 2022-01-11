{-|

Programming Languages
Fall 2020

Implementation in Haskell of the Structural Operational Semantics
described in Chapter 2 of Nielson & Nielson, Semantics with Applications

Author: Saúl García Martín
		Manuel González González

-}

module Exercises03 where

import           StructuralSemantics
import           While
import           WhileParser


-- |----------------------------------------------------------------------
-- | Exercise 1
-- |----------------------------------------------------------------------

-- | Given the type synonym 'DerivSeq' to represent derivation sequences
-- | of the structural operational semantics:

type DerivSeq = [Config]

-- | Define a function 'derivSeq' that given a WHILE statement 'st' and an
-- | initial state 's' returns the corresponding derivation sequence:

derivSeq :: Stm -> State -> DerivSeq
--derivSeq st ini = undefined

derivSeq (Ass x a) s = Inter (Ass x a) s : [sosStm (Inter (Ass x a) s)]

derivSeq Skip s = Inter Skip s : [sosStm (Inter Skip s)]

derivSeq (Comp s1 s2) s = Inter (Comp s1 s2) s : derivSeq ss s'
    where Inter ss s' = sosStm (Inter (Comp s1 s2) s)
      
derivSeq (If b s1 s2) s = Inter (If b s1 s2) s : derivSeq ss s'
    where Inter ss s' = sosStm (Inter (If b s1 s2) s)
    
derivSeq (While b ss) s = Inter (While b ss) s : derivSeq ss' s'
    where Inter ss' s' = sosStm (Inter (While b ss) s)

derivSeq (Repeat ss b) s = Inter (Repeat ss b) s : derivSeq ss' s'
    where Inter ss' s' = sosStm (Inter (Repeat ss b) s)

derivSeq (For x a1 a2 ss) s = Inter (For x a1 a2 ss) s : derivSeq ss' s'
    where Inter ss' s' = sosStm (Inter (For x a1 a2 ss) s)


-- | To test your definition of 'derivSeq' you can use the code below.
-- | The function 'facSeq' returns the derivation sequence of the 'factorial'
-- | statement executed from the initial state 'sInit':

facSeq :: DerivSeq
facSeq = derivSeq factorial sInit

-- | The function 'showDerivSeq' returns a String representation  of
-- | a derivation sequence 'dseq'. The 'vars' argument is a list of variables
-- | that holds the variables to be shown in the state:

showDerivSeq :: [Var] -> DerivSeq -> String
showDerivSeq vars dseq = unlines (map showConfig dseq)
  where
    showConfig (Final s)    = "Final state:\n" ++ unlines (showVars s vars)
    showConfig (Stuck ss s) = "Stuck state:\n" ++ show ss ++ "\n" ++ unlines (showVars s vars)
    showConfig (Inter ss s) = show ss ++ "\n" ++ unlines (showVars s vars)
    showVars s vs = map (showVal s) vs
    showVal s x = " s(" ++ x ++ ")= " ++ show (s x)

-- | Therefore, you can print the derivation sequence of 'factorial' with:

showFacSeq :: IO()
showFacSeq = putStrLn $ showDerivSeq ["x", "y"] facSeq

-- | You should get an output identical to 'derivSeqForFactorial.txt'

-- | The function 'sSoS' below is the semantic function of the
-- | structural operational semantics of WHILE. Given a WHILE statement 'st'
-- | and an initial state 's' returns the final configuration of the
-- | corresponding derivation sequence:

sSos :: Stm -> State -> State
sSos ss s = s'
  where Final s' = last (derivSeq ss s)

sFac' :: State
sFac' = sSos factorial sInit

-- |----------------------------------------------------------------------
-- | Exercise 2
-- |----------------------------------------------------------------------
-- | The WHILE language can be extended with a 'repeat S until b' statement.

-- | Exercise 2.1
-- | Define the structural operational semantics of this new statement. You
-- | are not allowed to rely on the 'while b do S' statement.

{- Formal definition of 'repeat S until b'


        [repeat-sos]  ------------------------------   ¿condition?

-}

-- | Exercise 2.2
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'repeat until' statement.




-- | Exercise 2.3
-- | Write a WHILE program to test your definition of the repeat statement.

repetir :: Stm
repetir = Comp (Ass "y" (N 1))
                 (Repeat 
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1))))
                          (Eq (V "x") (N 1)))

showRepSeq :: IO()
showRepSeq = putStrLn $ showDerivSeq ["x", "y"] (derivSeq repetir sInit)

-- |----------------------------------------------------------------------
-- | Exercise 3
-- |----------------------------------------------------------------------

-- | The WHILE language can be extended with a 'for x:= a1 to a2' statement.

-- | Exercise 3.1
-- | Define the structural operational semantics of this new statement. You
-- | are not allowed to rely on the 'while b do s' statement.

{- Formal definition of ''


        [for-sos]  ------------------------------   ¿condition?

-}

-- | Exercise 3.2
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'for' statement.


-- | Exercise 3.3
-- | Write a WHILE program to test your definition of the for statement.

repiteNVeces :: Stm
repiteNVeces = For "y" (N 1) (N 10) (Ass "x" (V "y"))

showRepNSeq :: IO()
showRepNSeq = putStrLn $ showDerivSeq ["x", "y"] (derivSeq repiteNVeces sInit)

-- |----------------------------------------------------------------------
-- | Exercise 4
-- |----------------------------------------------------------------------

-- | Extend WHILE with the 'Abort' statement.  The informal semantics of
-- | 'Abort' is to abruptly stop the execution of the program, similar to
-- | a call to 'exit(0)' in other mainstream languages.

-- | Exercise 4.1
-- | Modify the definition of 'sosStm' in 'StructuralSemantics.hs' to deal
-- | with the 'abort' statement.


-- | Exercise 4.2
-- | Define a function 'derivSeqAbort' similar to 'derivSeq' except
-- | that it deals with stuck configurations.

derivSeqAbort :: Stm -> State -> DerivSeq

derivSeqAbort (Ass x a) s = Inter (Ass x a) s : [sosStm (Inter (Ass x a) s)]

derivSeqAbort Skip s = Inter Skip s : [sosStm (Inter Skip s)]

derivSeqAbort (Comp s1 s2) s
  |isStuck ss2 = Inter (Comp s1 s2) s : [Stuck (Comp s1 s2) s]
  |otherwise = Inter (Comp s1 s2) s : derivSeqAbort ss s'
    where 
      ss2 = sosStm (Inter (Comp s1 s2) s)
      Inter ss s' = ss2
      
derivSeqAbort (If b s1 s2) s = Inter (If b s1 s2) s : derivSeqAbort ss s'
    where Inter ss s' = sosStm (Inter (If b s1 s2) s)
    
derivSeqAbort (While b ss) s = Inter (While b ss) s : derivSeqAbort ss' s'
    where Inter ss' s' = sosStm (Inter (While b ss) s)

derivSeqAbort (Repeat ss b) s = Inter (Repeat ss b) s : derivSeqAbort ss' s'
    where Inter ss' s' = sosStm (Inter (Repeat ss b) s)

derivSeqAbort (For x a1 a2 ss) s = Inter (For x a1 a2 ss) s : derivSeqAbort ss' s'
    where Inter ss' s' = sosStm (Inter (For x a1 a2 ss) s)

derivSeqAbort Abort s = Inter Abort s  : [Stuck Abort s]

-- | You can test your code with the examples below and the function
-- | 'showAbortSeq':

showAbortSeq :: IO()
showAbortSeq = putStrLn $ showDerivSeq ["x", "y"] (derivSeqAbort abortExample0 sInit)

abortExample0 :: Stm
abortExample0 = Abort

abortExample1 :: Stm
abortExample1 =  Comp (Ass "x" (N 1))
                (Comp (Ass "y" (N 2))
                      Abort)

abortExample2 :: Stm
abortExample2 =  Comp (Ass "x" (N 1))
                (Comp (Ass "y" (N 2))
                (Comp  Abort
                      (Ass "z" (N 3))))

abortExample3 :: Stm
abortExample3 = Comp (Ass "x" (N 1))
                     (While (Le (V "x") (N 5))
                         (If (Eq (V "x") (N 3))
                             Abort
                             (Ass "x" (Add (V "x") (N 1))))
                     )


-- | Convert concrete syntax to abstract syntax

concreteToAbstract :: String -> String -> IO()
concreteToAbstract inputFile outputFile =
  do
    (_, _, stm) <- parser inputFile
    let s = show stm              -- | have 'show' replaced by a pretty printer
    if null outputFile
      then putStrLn s
      else writeFile outputFile s

-- | Convert concrete syntax to String
-- | This allows to apply happy parsers and alex lexers to Strings as
-- | pure functions thus avoiding monads.

concreteToString :: String -> String -> IO()
concreteToString inputFile programName =
  do
    xs <- readFile inputFile
    let ys = map (\ l -> "   \\ " ++ l ++ "\\n\\") (lines xs)  -- | \n is there to support line comments in WHILE // ...
    putStrLn $ programName ++ " :: String"
    putStrLn $ programName ++ " ="
    putStrLn "   \"\\"
    mapM_ putStrLn ys
    putStrLn "   \\ \""

-- | Run the While program stored in filename and show final values of variables

run :: String -> IO()
run filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeq stm (const 0)
     putStr $ showDerivSeq vars dseq

runAbort :: String -> IO()
runAbort filename =
  do
     (_, vars, stm) <- parser filename
     let  dseq = derivSeqAbort stm (const 0)
     putStr $ showDerivSeq vars dseq
