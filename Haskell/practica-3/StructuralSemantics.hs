{-|

Programming Languages
Fall 2021

Implementation in Haskell of the Structural Operational Semantics
described in Chapter 2 of Nielson & Nielson, Semantics with Applications

Author: Manuel González González

-}

module StructuralSemantics where

import           Aexp
import           Bexp
import           While

data Update = Var :=>: Z

update :: State -> Update -> State
update s (x :=>: v) = \k -> if k==x then v else s k

-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s
            | Stuck Stm State  -- <S, s>

isFinal :: Config -> Bool
isFinal (Final _) = True
isFinal _         = False

isInter :: Config -> Bool
isInter (Inter _ _) = True
isInter _           = False

isStuck :: Config -> Bool
isStuck (Stuck _ _) = True
isStuck _           = False

-- representation of the transition relation <S, s> -> s'

sosStm :: Config -> Config

-- x := a

sosStm (Inter (Ass x a) s) =  Final (update s (x :=>: aVal a s))

-- skip

sosStm (Inter Skip s) = Final s

-- s1; s2

sosStm (Inter (Comp s1 s2) s)
            | isFinal stm = Inter s2 s'
                where
                    stm = sosStm (Inter s1 s)
                    Final s' = stm

sosStm (Inter (Comp s1 s2) s)
            | isInter stm = Inter (Comp s1' s2) s'
                where
                    stm = sosStm (Inter s1 s)
                    Inter s1' s' = stm

sosStm (Inter (Comp s1 s2) s)
            | isStuck stm = Stuck (Comp st s2) s'
            where
                stm = sosStm (Inter s1 s)
                Stuck st s' = stm

-- if b then s1 else s2

sosStm (Inter (If b s1 s2) s)
            | bVal b s = Inter s1 s
            | not (bVal b s) = Inter s2 s

-- while b do s

sosStm (Inter (While b st) s) = Inter (If b (Comp st (While b st)) Skip) s

-- repeat s until b

sosStm (Inter (Repeat st b) s) = Inter (Comp st (If b (Repeat st b) Skip)) s

-- for x a1 to a2 s

sosStm (Inter (For x a1 a2 st) s) = Inter (If b (Comp (Comp xStm st) (For x a1' a2' st)) Skip) s 
    where
        b = Le a1 a2
        xStm = Ass x a1
        a1' = N (aVal (Add a1 (N 1)) s)
        a2' = N (aVal a2 s)

-- abort

sosStm (Inter st s) = Stuck st s
