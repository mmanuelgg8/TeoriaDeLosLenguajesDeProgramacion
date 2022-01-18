module WhileExamples where

import           Aexp
import           Bexp
import           While

swap :: Stm
swap = Comp (Ass "z" (V "x"))
      (Comp (Ass "x" (V "y"))
            (Ass "y" (V "z")))

swapInit :: Var -> Z
swapInit "x" = 5
swapInit "y" = 7
swapInit _   = 0

factorial :: Stm
factorial = Comp (Ass "y" (N 1))
                 (While (Neg (Eq (V "x") (N 1)))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1)))))

factorialInit :: State
factorialInit "x" = 3
factorialInit _   = 0

-- program to illustrate the difference between dynamic and static scope

scope :: Stm
scope = Block (Dec "y" (N 0) -- located in 1
              (Dec "x" (N 0) -- located in 2
               EndDec))

              (Proc "p" (Ass "x" (Mult (V "x") (N 2)))
              (Proc "q" (Call "p")
               EndProc
              ))

              (Block (Dec "x" (N 5) -- located in 3
                      EndDec)
                     (Proc "p" (Ass "x" (Add (V "x") (N 1)))
                      EndProc)
                     (Comp
                         (Call "q")
                         (Ass "y" (V "x"))
                     )
              )


-- recursive program to compute the factorial of 'n'

recFactorial :: Z -> Stm
recFactorial n =
          Block (Dec "y" (N 0) -- located in 1
                (Dec "x" (N n) -- located in 2
                 EndDec))

                (Proc "fac"
                (Block (Dec "z" (V "x") -- located from 3 to ...
                        EndDec)
                        EndProc

                       (If (Eq (V "x") (N 1))
                           Skip
                           (Comp (Ass "x" (Sub (V "x") (N 1)))
                           (Comp (Call "fac")
                                 (Ass "y" (Mult (V "z") (V "y")))
                           ))))
                 EndProc)

                (Comp (Ass "y" (N 1))
                      (Call "fac")
                )
