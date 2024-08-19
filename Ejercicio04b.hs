--
-- ejercicio 4 (b)
--

{-# LANGUAGE GADTs, KindSignatures #-}

module Ejercicio04b where

import Prelude hiding (lookup)
import Data.Map ( Map, fromList, lookup, empty, insert )

--
-- deep embedding
--

type Trivalued = Maybe Bool
type Ctx = Map String Bool

data TCtx :: * -> * where
    Empty :: TCtx Ctx
    Def   :: String -> Bool -> TCtx Ctx -> TCtx Ctx

data TExpr :: * -> * where
    Val :: Int -> TExpr Int
    Eq  :: TExpr Int -> TExpr Int -> TExpr Trivalued
    Lt  :: TExpr Int -> TExpr Int -> TExpr Trivalued
    Not :: TExpr Trivalued -> TExpr Trivalued
    And :: TExpr Trivalued -> TExpr Trivalued -> TExpr Trivalued
    Or  :: TExpr Trivalued -> TExpr Trivalued -> TExpr Trivalued
    Var :: String -> TCtx Ctx -> TExpr Trivalued

--
-- interpretaciones
--

build :: TCtx c -> c
build Empty = empty
build (Def s b c) = insert s b (build c)

eval :: TExpr t -> t
eval (Val n) = n
eval (Eq x y) = Just (eval x == eval y)
eval (Lt x y) = Just (eval x < eval y)
eval (Not a) = do not <$> eval a
eval (And a b) = do (&&) <$> eval a <*> eval b
eval (Or a b) = do (||) <$> eval a <*> eval b
eval (Var s c) = lookup s (build c)

pprint :: TExpr t -> String
pprint (Val x) = show x
pprint (Eq x y) = "(" ++ pprint x ++ " = " ++ pprint y ++ ")"
pprint (Lt x y) = "(" ++ pprint x ++ " < " ++ pprint y ++ ")"
pprint (Not (And x y)) = "~" ++ "(" ++ pprint (And x y) ++ ")"
pprint (Not (Or x y))  = "~" ++ "(" ++ pprint (Or x y) ++ ")"
pprint (Not a)         = "~" ++ pprint a
pprint (And (Or a b) (Or c d)) = "(" ++ pprint (Or a b) ++ ") /\\ (" ++ pprint (Or c d) ++ ")"
pprint (And (Or a b) c)        = "(" ++ pprint (Or a b) ++ ") /\\ " ++ pprint c
pprint (And a        (Or b c)) = pprint a ++ " /\\ (" ++ pprint (Or b c) ++ ")"
pprint (And a        b)        = pprint a ++ " /\\ " ++ pprint b
pprint (Or a b) = pprint a ++ " \\/ " ++ pprint b
pprint (Var s c) = "[" ++ s ++ " : " ++ show (lookup s (build c)) ++ "]"

--
-- ejemplo
--

program = 
    And (Lt (Val 2) (Val 3))
         (Not (And (Var "x" c)
                     (Var "y" c)))
    where
        c =  Def "x" True (Def "y" False Empty)
result = pprint program
main = putStrLn result
