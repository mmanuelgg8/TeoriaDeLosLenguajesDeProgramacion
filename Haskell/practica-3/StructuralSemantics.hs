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
            | isFinal (sosStm (Inter s1 s)) = Inter s2 s'
                where
                    Final s' = sosStm (Inter s1 s)
                    
sosStm (Inter (Comp s1 s2) s)                    
            | isInter (sosStm (Inter s1 s)) = Inter (Comp s1 s2) s'
                where
                    Inter s1' s' = sosStm (Inter s1 s)
-- if b then s1 else s2

sosStm (Inter (If b s1 s2) s)
            | bVal b s = Inter s1 s
            | not (bVal b s) = Inter s2 s

-- while b do s

sosStm (Inter (While b st) s) = Inter (If b (Comp st (While b st)) Skip) s

-- repeat s until b

-- todo

-- for x a1 to a2 s

-- todo

-- abort

-- todo
