--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n-1) (\v->k(n*v))
{-
factk n k= aux n 1 k
    where aux 1 a k= k a
          aux n a k= aux (n-1) (\v-> k$ v *(a*n)) 
-}
--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t

evenhelper xx k1 k2= aux xx k1 k2
    where aux [] k1 k2=k1 0
          aux (x:xs) k1 k2| even x= aux xs (\v-> k1$ v+x) k2  
                          | odd  x= aux xs (\v-> k1$ v+0) k2 
oddhelper xx k1 k2= aux xx k1 k2
    where aux [] k1 k2=k2 0
          aux (x:xs) k1 k2| odd x= aux xs k1 (\v-> k2$ v+x)  
                          | even x= aux xs k1 (\v-> k2$ v+0) 

evenoddk xx k1 k2 = case (even$last xx) of True -> evenhelper xx k1 k2
                                           False-> oddhelper xx k1 k2 

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple e = case e of 
    IntExp i-> True
    VarExp s-> True
    OpExp op e1 e2-> isSimple e1 && isSimple e2
    IfExp c t f -> isSimple c && isSimple t && isSimple f
    AppExp _ _ -> False

---isSimple (OpExp "+" (IntExp 10) (AppExp (VarExp "f") (VarExp "v")))

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions

cpsExp (IntExp i) k n= (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n= (AppExp k (VarExp v), n)

-----cpsExp (VarExp "y") (VarExp "k") 1
-----(k y,1)
--cpsExpRun :: String -> Exp
--cpsExpRun input = fst $ cpsExp (toExp input) (VarExp "k") 1

---putStrLn$ ctorParse "x+1"

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k i | isSimple e= (AppExp (AppExp f e) k, i)
                        | otherwise = let (v, n) = gensym i
                                       in let lambody = (AppExp (AppExp f (VarExp v))) k
                                           in cpsExp e (LamExp v lambody) n

--- cpsExp (AppExp (VarExp "f")(OpExp "+" (VarExp "g")(IntExp 3))) (VarExp "k") 1
--- cpsExp (AppExp (VarExp "f") (AppExp (VarExp "g") (AppExp (VarExp "h") (VarExp "i")))) (VarExp "k") 1
--- isSimple (OpExp "+" (VarExp "g")(IntExp 3))

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), n)
                            | isSimple e1 == False && (isSimple e2) = let (v, i) = gensym n
                                                                       in let lambody = (AppExp k (OpExp op (VarExp v) e2 ))
                                                                           in cpsExp e1 (LamExp v lambody) i
                            | isSimple e1 && (isSimple e2) == False = let (v, i) = gensym n
                                                                       in let lambody = (AppExp k (OpExp op e1 (VarExp v))) 
                                                                           in cpsExp e2 (LamExp v lambody) i
                            | otherwise = let (v1, i1) = gensym n
                                              (v2, i2) = gensym i1
                                           in let (lambody, i3) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) i2
                                               in cpsExp e1 (LamExp v1 lambody) i3
---cpsExp (OpExp "<" (AppExp (VarExp "f") (VarExp "a")) (AppExp (VarExp "g") (VarExp "q")))) (VarExp "k") 1
---cpsExp (OpExp "*" (IntExp 4) (AppExp (VarExp "f") (VarExp "a"))) (VarExp "k") 4 
---cpsExp (OpExp "*"  (AppExp (VarExp "f") (VarExp "a"))(IntExp 4)) (VarExp "k") 1
---cpsExp (OpExp "<" (IntExp 5) (OpExp "+" (IntExp 3) (AppExp (VarExp "g") (VarExp "q")))) (VarExp "k") 1
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp c t f) k n |isSimple c =  let (cpst, i1) = cpsExp t k n
                                            (cpsf, i2) = cpsExp f k i1
                                         in ( IfExp c cpst cpsf , i2)
                         |otherwise  =  let (v1, i1) = gensym n
                                            (cpst, i2) = cpsExp t k i1
                                            (cpsf, i3) = cpsExp f k i2
                                         in cpsExp c (LamExp v1 (IfExp (VarExp v1) cpst cpsf)) i3
---putStrLn$ ctorParse "if 3 < 5 then 44 + x else 14 * 2"
---IfExp (OpExp "<" (IntExp 3) (IntExp 5)) (OpExp "+" (IntExp 44) (VarExp "x")) (OpExp "*" (IntExp 14) (IntExp 2))
---putStrLn$ ctorParse "if p (3 + a) then 15 + 7 else 12"
--"p (3 + a) (\\v1 -> (if v1 then k (15 + 7) else k 12))"
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
--1. add a k to you parameters list
--2. transform the function body as CPS style which use the cpsExp u defined as above 
cpsDecl (Decl f ps body)= Decl f (ps++["k"]) (fst$(cpsExp body (VarExp "k") 1))
{-
cpsDecl (Decl f args e) =
    let (cpse,_) = cpsExp e (VarExp "k") 1
    in Decl f (args++["k"]) cpse
    -}