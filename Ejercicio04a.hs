--
-- ejercicio 4 (a)
--

-- nota: decidí incorporar la declaración de variables conjunto a 
--       las variables en sí. Por como está expresado el enunciado,
--       me pareció que esa era la intención. 
--       En base a esto, y siguiendo un diseño de GLC con atributos 
--       heredados para resolver el valor de las variables, armé
--       un edsl para la declaración de variables independiente
--       a TExpr.

import Prelude hiding (lookup)
import Data.Map

--
-- Shallow embedding
--

type Trivalued = Maybe Bool
type Ctx = Map String Bool

class TCtx c where
    emptyT :: c
    defT :: String -> Bool -> c -> c

class TExpr e where
    valT :: Int -> e Int
    eqT  :: e Int -> e Int -> e Trivalued
    ltT  :: e Int -> e Int -> e Trivalued
    notT :: e Trivalued -> e Trivalued
    andT :: e Trivalued -> e Trivalued -> e Trivalued
    orT  :: e Trivalued -> e Trivalued -> e Trivalued
    varT :: String -> Ctx -> e Trivalued

instance TCtx Ctx where
    emptyT = empty
    defT = insert

newtype TEval t = TE t
instance TExpr TEval where
    valT = TE
    eqT (TE x) (TE y) = TE (Just (x == y))
    ltT (TE x) (TE y) = TE (Just (x < y))
    notT (TE a) = TE (not <$> a)
    andT (TE a) (TE b) = TE ((&&) <$> a <*> b)
    orT (TE a) (TE b) = TE ((||) <$> a <*> b)
    varT s c = TE (lookup s c)

pprint :: Show t => TEval t -> IO()
pprint (TE a) = print a

--
-- ejemplo
--

result =
    andT (ltT (valT 2) (valT 3))
         (notT (andT (varT "x" c)
                     (varT "y" c)))
    where
        c =  defT "x" True (defT "y" False emptyT)
main = pprint result
