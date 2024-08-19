--
-- ejercicio 3 (a) deep embedding
--

{-# LANGUAGE GADTs, KindSignatures #-}

module Ejercicio03aDeep where
import Ejercicio02 hiding (eval, program, result, main)

eval :: TExpr t -> String
eval (Val x) = show x
eval (Eq x y) = "(" ++ eval x ++ " = " ++ eval y ++ ")"
eval (Lt x y) = "(" ++ eval x ++ " < " ++ eval y ++ ")"
eval (Not (And x y)) = "~" ++ "(" ++ eval (And x y) ++ ")"
eval (Not (Or x y))  = "~" ++ "(" ++ eval (Or x y) ++ ")"
eval (Not a)         = "~" ++ eval a
eval (And (Or a b) (Or c d)) = "(" ++ eval (Or a b) ++ ") /\\ (" ++ eval (Or c d) ++ ")"
eval (And (Or a b) c)        = "(" ++ eval (Or a b) ++ ") /\\ " ++ eval c
eval (And a        (Or b c)) = eval a ++ " /\\ (" ++ eval (Or b c) ++ ")"
eval (And a        b)        = eval a ++ " /\\ " ++ eval b
eval (Or a b) = eval a ++ " \\/ " ++ eval b

--
-- ejemplo
--

program =
    And (Or (Lt (Val 2) (Val 3)) 
            (Not (Eq (Val 3) (Val 5)))) 
        (And (Or (Lt (Val 2) (Val 3))
                (Or (Eq (Val 0) (Val 1))
                        (Lt (Val 3) (Val 2))))
            (Eq (Val 0) 
                (Val 1)))
result = eval program
main = putStrLn result
