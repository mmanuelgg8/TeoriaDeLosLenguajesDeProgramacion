module Aexp where

data Secuencia = Empty | Node Int Secuencia deriving Show
data Aexp = Number Integer | Var String | PLUS Aexp Aexp | BY Aexp Aexp | LESS Aexp Aexp deriving Show

exp1 :: Aexp
exp1 = BY (PLUS (Var "x")(Number 1)) (LESS (Var "y")(Number 5))

-- n :: = 0 | 1 | n0 | n1

data NumBin = Cero | Uno | N0 NumBin | N1 NumBin deriving Show

-- 1101

ejbin :: NumBin
ejbin = N1 (N0 (N1 Uno))

n :: NumBin -> Integer
n Cero = 0
n Uno = 1
n (N0 b) = 2 * n b
n (N1 b) = 2 * n b + 1


