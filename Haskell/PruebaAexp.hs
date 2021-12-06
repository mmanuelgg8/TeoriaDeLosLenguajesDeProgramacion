module Aexp where

-- a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2

data Aexp = Lit String
        |   Var String 
        |   Add Aexp Aexp
        |   Prod Aexp Aexp
        |   Sub Aexp Aexp
        deriving Show

e = "3*x + 6 - y"

e1 :: Aexp
e1 = Sub (Add (Prod (Lit "3") (Var "x")) (Lit "6")) (Var "y")

-- n ::= 0 | 1 | n 0 | n 1

-- N[1010]

data Binary = O | I | OE Binary | IE Binary
type Z = Integer 
type Var = String
type State = Var -> Z

s0 :: State
s0 "x" = 3
s0 "y" = 5
s0 _ = 0

eval :: Aexp -> State -> Z
eval (Lit n) _ = read n
eval (Var x) s = s x
eval (Add a1 a2) s = eval a1 s + eval a2 s
eval (Prod a1 a2) s = eval a1 s * eval a2 s 
eval (Sub a1 a2) s = eval a1 s - eval a2 s


