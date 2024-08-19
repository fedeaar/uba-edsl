--
-- ejercicio 3 (b) 
--

{-# LANGUAGE GADTs, KindSignatures #-}

module Ejercicio03b where

data UProp :: * where
    Val :: Int -> UProp
    Eq  :: UProp -> UProp -> UProp
    Lt  :: UProp -> UProp -> UProp
    Not :: UProp -> UProp 
    And :: UProp -> UProp -> UProp
    Or  :: UProp -> UProp -> UProp

--
-- versión del ej. 03 (a) deep embedding, pero sin tipos, para el ej. 03 (c)
--

eval :: UProp -> String
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
