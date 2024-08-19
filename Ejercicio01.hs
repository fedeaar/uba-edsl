--
-- ejercicio 1 
--

module Ejercicio01 where

class TExpr e where
    valT :: Int -> e Int
    eqT  :: e Int -> e Int -> e Bool
    ltT  :: e Int -> e Int -> e Bool
    notT :: e Bool -> e Bool
    andT :: e Bool -> e Bool -> e Bool
    orT  :: e Bool -> e Bool -> e Bool

newtype TEval t = TE t

instance TExpr TEval where
    valT = TE
    eqT (TE x) (TE y) = TE (x == y)
    ltT (TE x) (TE y) = TE (x < y)
    notT (TE a) = TE (not a)
    andT (TE a) (TE b) = TE (a && b)
    orT (TE a) (TE b) = TE (a || b)

pprint :: Show t => TEval t -> IO()
pprint (TE a) = print a

--
-- ejemplo
--

result = andT (ltT (valT 2) (valT 3)) (notT (eqT (valT 3) (valT 5)))
main = pprint result
