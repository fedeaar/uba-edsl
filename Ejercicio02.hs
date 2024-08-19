--
-- ejercicio 2 
--

{-# LANGUAGE GADTs, KindSignatures #-}

module Ejercicio02 where

data TExpr :: * -> * where
    Val :: Int -> TExpr Int
    Eq  :: TExpr Int -> TExpr Int -> TExpr Bool
    Lt  :: TExpr Int -> TExpr Int -> TExpr Bool
    Not :: TExpr Bool -> TExpr Bool
    And :: TExpr Bool -> TExpr Bool -> TExpr Bool
    Or  :: TExpr Bool -> TExpr Bool -> TExpr Bool

eval :: TExpr t -> t
eval (Val n) = n
eval (Eq x y) = eval x == eval y
eval (Lt x y) = eval x < eval y
eval (Not a) = not (eval a)
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b

--
-- ejemplo
--

program = And (Lt (Val 2) (Val 3)) (Not (Eq (Val 3) (Val 5)))
result = eval program
main = print result
