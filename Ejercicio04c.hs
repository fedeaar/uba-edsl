--
-- ejercicio 4 (c)
--

module Ejercicio04c where

import MonParsing
import MonParsingExt

--
-- extensión de UProp
--

data UProp :: * where
    Val :: Int -> UProp
    Eq  :: UProp -> UProp -> UProp
    Lt  :: UProp -> UProp -> UProp
    Not :: UProp -> UProp
    And :: UProp -> UProp -> UProp
    Or  :: UProp -> UProp -> UProp
    Var :: String -> UCtx -> UProp

data UCtx :: * where
    Empty :: UCtx
    Def   :: String -> Bool -> UCtx -> UCtx

type PData = (UCtx, UProp)

--
-- Parser UProp
--

-- nota: para agregar las variables proposicionales, incorporé los  
--       siguientes cambios a la sintaxis:
--       - program  ::= varList '$' prop
--       - varList  ::= varDecl varList' | Empty
--       - varList' ::= ',' varList' | Empty
--       - varDecl  ::= var ':=' bool
--       - var      ::= alphaNumeric+
--       - bool     ::= 'True' | 'False'
--       - factor   ::= ... | var

pProgram :: Parser UProp
pProgram =
    do
        a <- pVarList
        pString "$"
        pProp a

--
-- UCtx
--

pVarList :: Parser UCtx
pVarList =
    do
        (s, b) <- pVarDecl
        Def s b <$> pVarList'
    <|>
    return Empty

pVarList' :: Parser UCtx
pVarList' =
    do
        pString ","
        (s, b) <- pVarDecl
        Def s b <$> pVarList'
    <|>
    return Empty

pVarDecl :: Parser (String, Bool)
pVarDecl =
    do
        s <- pVariable
        pString ":="
        b <- pBool
        return (s, b)

--
-- UProp
--

pProp :: UCtx -> Parser UProp
pProp c =
    (do
        a <- pTerm c
        pString "\\/"
        Or a <$> pProp c)
    <|>
    pTerm c

pTerm :: UCtx -> Parser UProp
pTerm c =
    (do
        a <- pFactor c
        pString "/\\"
        And a <$> pTerm c)
    <|>
    pFactor c

pFactor :: UCtx -> Parser UProp
pFactor c = pNot c <|> pParenProp c <|> pEq <|> pLt <|> pVal <|> pVar c

pNot :: UCtx -> Parser UProp
pNot c =
    do
        pString "~"
        Not <$> pProp c

pParenProp :: UCtx -> Parser UProp
pParenProp c =
    do
        pString "("
        p <- pProp c
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

pVar :: UCtx -> Parser UProp
pVar c =
    do
        s <- pVariable
        return (Var s c)
