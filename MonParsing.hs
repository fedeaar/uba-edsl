--
-- Parsers tomados de mon-parsing.hs (alterados levemente)
--

module MonParsing where

import Control.Monad
import GHC.Base hiding ((<|>))

newtype Parser a = P {runP :: String -> [(a,String)]}

instance Functor Parser where
    fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

instance Applicative Parser where
    pure a =  P (\cs -> [(a,cs)])
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f, cs')   <- p cs
                                                ,  (a, cs'')  <- q cs']

instance Monad Parser where
  (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a, cs') <- p cs]

pFail :: Parser a
pFail = P $ const []

(<|>) :: Parser a -> Parser a -> Parser a
(P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
                              []     -> []
                              (x:xs) -> [x]

item :: Parser Char
item = P $ \cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)]

pSat :: (Char -> Bool) -> Parser Char
pSat p = do c <- item
            if p c then return c
                   else pFail

pSym :: Char -> Parser Char
pSym c = pSat (== c)

pList :: Parser a -> Parser [a]
pList p = do a <- p
             as <- pList p
             return (a:as)
          <|>
          return []

pListP :: Parser a -> Parser [a]
pListP p = do a <- p
              as <- pList p
              return (a:as)

digit :: Parser Int
digit = do c <- pSat isDigit
           return (ord c - ord '0')

isDigit c = (c >= '0') && (c <= '9')

number :: Parser Int
number = do d <- digit
            number' d

number' :: Int -> Parser Int
number' n = do d <- digit
               number' (n*10 + d)
            <|>
            return n
