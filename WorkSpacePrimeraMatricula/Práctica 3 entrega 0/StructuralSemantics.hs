{-|

Programming Languages
Fall 2020

Implementation in Haskell of the Structural Operational Semantics
described in Chapter 2 of Nielson & Nielson, Semantics with Applications

Author: Manuel González González
        Saúl García Martín

-}

module StructuralSemantics where

import           While

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

isFinal :: Config -> Bool
isFinal (Final _)    = True
isFinal _            = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

-- representation of the transition relation <S, s> -> s'

getStateFromConfig :: Config -> State
getStateFromConfig (Final s) = s
getStateFromConfig (Inter _ s) = s

getStmFromConfig :: Config -> Stm
getStmFromConfig (Inter ss _) = ss 

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) = Final (update s x (aVal a s))
  where
    update s x v y = if x == y then v else s y

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2

sosStm (Inter (Comp s1 s2) s)
  |isFinal ss = Inter s2 s'
    where 
      ss = sosStm (Inter s1 s)
      s' = getStateFromConfig ss

sosStm (Inter (Comp s1 s2) s)
  |isInter ss = Inter (Comp s1' s2) s'
    where 
      ss = sosStm (Inter s1 s)
      s' = getStateFromConfig ss
      s1' = getStmFromConfig ss

{- sosStm (Inter (Comp s1 s2) s)
  |isStuck ss = sosStm (Inter (Comp s)) -}

-- if b then s1 else s2

-- B[b]s == tt

sosStm (Inter (If b ss1 ss2) s)
  | bVal b s = Inter ss1 s
  
-- B[b]s == ff

sosStm (Inter (If b ss1 ss2) s)
  | bVal (Neg b) s = Inter ss2 s

-- while b do s

sosStm (Inter (While b ss) s) = Inter (If b (Comp ss (While b ss)) Skip) s

-- repeat s until b

sosStm (Inter (Repeat ss b) s) = Inter (If b Skip (Comp ss (Repeat ss b))) s

-- for x a1 to a2 s

sosStm (Inter (For x a1 a2 ss) s) = Inter (If b (Comp (Comp s' ss) (For x a3 a2 ss)) Skip) s
  where
    b = Le a1 a2
    s' = Ass x a1
    a3 = N (aVal (Add a1 (N 1)) s)

-- abort

sosStm (Inter Abort s) = Stuck Abort s
