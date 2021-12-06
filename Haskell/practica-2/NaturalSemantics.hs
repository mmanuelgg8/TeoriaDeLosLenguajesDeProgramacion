{-|

Programming Languages
Fall 2021

Implementation of the Natural Semantics of the While Language

Reference: Chapter 2 of Nielson & Nielson, "Semantics with Applications"

Author: Manuel González González

-}

module NaturalSemantics where

import           Aexp
import           Bexp
import           While

import           WhileExamples
import           Exercises01
import           Test.HUnit hiding (State)
-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s)      = Final (update s (x :=>: aVal a s))

-- skip

nsStm (Inter Skip s)           = Final s

-- s1; s2

--nsStm (Inter (Comp ss1 ss2) s) = Final (sNs ss2 (sNs ss1 s))

nsStm (Inter (Comp ss1 ss2) s) = Final s''
  where
    Final s' = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s')

{-
nsStm (Inter (Comp ss1 ss2) s) =
  let
    Final s' = nsStm (Inter ss1 s)
    Final s'' = nsStm (Inter ss2 s)
  in
    Final s''
-}

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s)
  | bVal b s = nsStm (Inter ss1 s)

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s)
  | not (bVal b s)  = nsStm (Inter ss2 s)

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s)
  | not (bVal b s) = Final s

-- B[b]s = tt
nsStm (Inter (While b ss) s)
  | bVal b s = nsStm (Inter (While b ss) s')
  where
    Final s' = nsStm (Inter ss s)

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

testNS :: Test
testNS = test [
              "Ejecución de swap x" ~: 7 ~=? sNs swap swapInit "x",
              "Ejecución de factorial" ~: 6 ~=? sNs factorial factorialInit "y"]