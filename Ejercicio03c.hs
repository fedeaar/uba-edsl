--
-- ejercicio 3 (c) 
--

module Ejercicio03c where

import MonParsing
import MonParsingExt
import Ejercicio03b

--
-- Parser UProp
--

pProp :: Parser UProp
pProp =
    (do
        a <- pTerm
        pString "\\/"
        Or a <$> pProp)
    <|>
    pTerm

pTerm :: Parser UProp
pTerm =
    (do
        a <- pFactor
        pString "/\\"
        And a <$> pTerm)
    <|>
    pFactor

pFactor :: Parser UProp
pFactor = pNot <|> pParenProp <|> pEq <|> pLt <|> pVal

pNot :: Parser UProp
pNot =
    do
        pString "~"
        Not <$> pProp

pParenProp :: Parser UProp
pParenProp =
    do
        pString "("
        p <- pProp
        pString ")"
        return p

pEq :: Parser UProp
pEq =
    do
        pString "("
        a <- pVal
        pString "="
        b <- pVal
        pString ")"
        return (Eq a b)

pLt :: Parser UProp
pLt =
    do
        pString "("
        a <- pVal
        pString "<"
        b <- pVal
        pString ")"
        return (Lt a b)


pVal :: Parser UProp
pVal = do Val <$> pNumber

--
-- ejemplo
--

source = "((2 < 3) \\/ ~(3 = 5)) /\\ ((2 < 3) \\/ (0 = 1) \\/ (3 < 2)) /\\ (0 = 1)"
parsed = runP pProp source
interp = map (eval . fst) parsed
printable = 
    ("source : " ++ source ++ "\n") ++
    foldl (\x c -> "pprint : " ++ c ++ "\n" ++ x) 
          "" 
          interp
main = putStrLn printable
