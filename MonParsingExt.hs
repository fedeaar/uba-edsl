--
-- parsers primitivos nuevos
--

module MonParsingExt where

import Data.Char (isAlphaNum)
import MonParsing

pSpace :: Parser String
pSpace = pList pSpace'

pSpace' :: Parser Char
pSpace' = pSym ' ' <|> pSym '\n' <|> pSym '\t' <|> pSym '\r'

pBound :: Parser t -> Parser t
pBound p =
    do
        pSpace
        a <- p
        pSpace
        return a

pString :: String -> Parser String
pString s = pBound (pString' s)

pString' :: String -> Parser String
pString' []     = return []
pString' (x:xs) =
    do
        c <- pSym x
        cs <- pString' xs
        return (c:cs)

pNumber :: Parser Int
pNumber = pBound number

pVariable :: Parser String
pVariable = pListP pAlpha

pAlpha :: Parser Char
pAlpha = pSat isAlphaNum

pBool :: Parser Bool
pBool =
    do
        pString "True"
        return True
    <|>
    do
        pString "False"
        return False
