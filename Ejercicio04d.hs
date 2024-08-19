--
-- ejercicio 4 (d)
--

{-# LANGUAGE GADTs, KindSignatures #-}

import MonParsing
import Ejercicio04b as NsTExpr hiding (main)
import Ejercicio04c as NsUProp

data Ty :: * -> * where
    TInt :: Ty Int
    TBool :: Ty Trivalued
    TString :: Ty String
    TCtx :: Ty Ctx

typeCtx :: Ty t -> UCtx -> TCtx t
typeCtx TCtx NsUProp.Empty = NsTExpr.Empty
typeCtx TCtx (NsUProp.Def s b c) = NsTExpr.Def s b (typeCtx TCtx c)

typeProp :: Ty t -> UProp -> TExpr t
typeProp TInt  (NsUProp.Val x) = NsTExpr.Val x
typeProp TBool (NsUProp.Eq x y) = NsTExpr.Eq (typeProp TInt x) (typeProp TInt y)
typeProp TBool (NsUProp.Lt x y) = NsTExpr.Lt (typeProp TInt x) (typeProp TInt y)
typeProp TBool (NsUProp.Not a) = NsTExpr.Not (typeProp TBool a)
typeProp TBool (NsUProp.And a b) = NsTExpr.And (typeProp TBool a) (typeProp TBool b)
typeProp TBool (NsUProp.Or a b) = NsTExpr.Or (typeProp TBool a) (typeProp TBool b)
typeProp TBool (NsUProp.Var s c) = NsTExpr.Var s (typeCtx TCtx c)

--
-- ejemplo
--

source = "x := True, y := False $ ((2 < 3) \\/ ~(3 = 5)) /\\ ((2 < 3) \\/ (0 = 1) \\/ y) /\\ x"
parsed = runP pProgram source
typed = map (typeProp TBool . fst) parsed
interp = map (\ x -> (pprint x, eval x)) typed
printable =
    ("source : " ++ source ++ "\n") ++
    foldl (\x (p, r) -> "pprint : " ++ p ++ ", eval : " ++ show r ++ "\n" ++ x)
          ""
          interp
main = putStrLn printable
