{-|

Programming Languages
Fall 2020

Implementation in Haskell of the Natural Semantics described in Chapter 2 of
Nielson & Nielson, Semantics with Applications

Authors: Saúl García Martín y Manuel González González

-}

module NaturalSemantics where

import           While
import           Exercises01      (Update (..), fvAexp, fvBexp, update)

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

--nsStm (Inter (Ass x a) s)      = Final (\k -> if k==x then aVal a s else s k)
nsStm (Inter (Ass x a) s)      = Final (update s (x :=>: aVal a s))

-- skip

nsStm (Inter Skip s)           = Final s

-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = nsStm (Inter ss2 s2)
  where Final s2 = nsStm (Inter ss1 s)

-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s)
  | bVal b s = nsStm (Inter ss1 s)

-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s)
  | bVal (Neg b) s = nsStm (Inter ss2 s)

-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s)
  | bVal (Neg b) s = Final s

-- B[b]s = tt
nsStm (Inter (While b ss) s)  
  | bVal b s = nsStm (Inter (While b ss) s2)
  where Final s2 = nsStm (Inter ss s)

-- repeat s until b

-- B[b]s = ff
nsStm (Inter (Repeat ss b) s)
  | bVal (Neg b) s = nsStm (Inter (Repeat ss b) s2)
  where Final s2 = nsStm (Inter ss s)

-- B[b]s = tt
nsStm (Inter (Repeat ss b) s)
  | bVal b s = Final s

-- for v a1 a2 s

-- inside the loop
nsStm (Inter (For v a1 a2 ss) s) 
  | bVal (Le a1 a2) s = nsStm (Inter (For v (Add a1 (N 1)) a2 ss) s2)
  where Final s2 = nsStm (Inter ss (update s (v :=>: aVal a1 s)))

-- loop ends
nsStm (Inter (For v a1 a2 ss) s) 
  | bVal (Neg(Le a1 a2)) s = Final s

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

-- Example C.1
sFac :: State
sFac = sNs factorial sInit
-- End Example C.1
