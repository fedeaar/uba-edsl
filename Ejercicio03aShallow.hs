--
-- ejercicio 3 (a) shallow embedding
--

import Ejercicio01 hiding (pprint, result, main)

-- nota: agrego tags (a la interpretación) para tipar por no terminales de
--       la sintaxis. Así, puedo definir la regla Factor := '(' Prop ')'.
--       Dado que no reflejan tipos de Haskell, no se me ocurre cómo dar 
--       una alternativa completamente tagless.

data TSyntax = TProp 
             | TTerm
             | TFactor
data TPrint t = TP TSyntax String

instance TExpr TPrint where
    valT x = TP TFactor (show x)
    eqT (TP _ x) (TP _ y) = TP TFactor ("(" ++ x ++ " = " ++ y ++ ")")
    ltT (TP _ x) (TP _ y) = TP TFactor ("(" ++ x ++ " < " ++ y ++ ")")
    notT (TP TFactor a) = TP TFactor ("~" ++ a)
    notT (TP _ a)       = TP TFactor ("~" ++ "(" ++ a ++ ")")
    andT (TP TProp a) (TP TProp b) = TP TTerm ("(" ++ a ++ ") /\\ (" ++ b ++ ")")
    andT (TP TProp a) (TP _ b)     = TP TTerm ("(" ++ a ++ ") /\\ " ++ b)
    andT (TP _ a)     (TP TProp b) = TP TTerm (a ++ " /\\ (" ++ b ++ ")")
    andT (TP _ a)     (TP _ b)     = TP TTerm (a ++ " /\\ " ++ b)
    orT (TP _ a) (TP _ b) = TP TProp (a ++ " \\/ " ++ b)

pprint :: Show t => TPrint t -> IO()
pprint (TP _ t) = putStrLn t 

--
-- ejemplo
--

result = 
    andT (orT (ltT (valT 2) (valT 3)) 
              (notT (eqT (valT 3) (valT 5)))) 
         (andT (orT (ltT (valT 2) (valT 3))
                    (orT (eqT (valT 0) (valT 1))
                         (ltT (valT 3) (valT 2))))
               (eqT (valT 0) 
                    (valT 1)))
main = pprint result
