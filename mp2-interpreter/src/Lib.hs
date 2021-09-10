module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
                                 
--liftIntOp div (IntVal x) (IntVal 0) = ExnVal "Division by 0"  ----how to let haskell know it is a div 
liftIntOp op  (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"       ----------Q: why i cannont do liftIntOp _ _ _ = ExnVal "Cannot lift"  I can                        

--IntOpExp "*" (IntExp 0) (IntExp 0)  --0

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp f (BoolVal x) (BoolVal y) = BoolVal $ f x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp f (IntVal x) (IntVal y) = BoolVal $ f x y
liftCompOp _ _ _ = ExnVal "Cannot lift"


eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of
      Just s   -> s
      Nothing  -> ExnVal "No match in env"

--- ### Arithmetic
--- Why this failed? V2=eval e2 env -> IntVal 0
---eval(IntOpExp "/" (IntOpExp "-" (IntExp 3) (IntExp (-8)))(IntOpExp "/" (IntOpExp "*" (IntExp 1) (IntExp (-1))) (IntExp (-3)))) H.empty
eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op intOps
  in if op == "/" && v2 == (IntVal 0)
     then ExnVal "Division by 0"
     else liftIntOp f v1 v2
{-
eval (IntOpExp op e1 e2) env = 
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op intOps
          in case (op, v1, v2) of 
              (("/"),_, (IntVal 0)) -> ExnVal "Division by 0" 
              (_,_,_)->  liftIntOp f v1 v2 -}
----this will fail?
----eval(IntOpExp "/" (IntOpExp "-" (IntExp 3) (IntExp (-8))) (IntOpExp "/" (IntOpExp "*" (IntExp 1) (IntExp (-1))) (IntExp (-3))))
{-  
eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op intOps
  in if op == "/" && v2 == (IntVal 0)
     then ExnVal "Division by 0"
     else liftIntOp f v1 v2
---   in (if op== "/" && v2== (IntVal 0) then ExnVal "Division by 0" else liftIntOp f v1 v2)
-}           
 
---Q1: why i have to :r instead just define the fn again in the termial and test will fail with Non-exhaustive pattern        
------working suddenly if i reload the modules instead of running it in terminal                                   


--- ### Boolean and Comparison Operators
eval (BoolOpExp op e1 e2) env=
  let v1= eval e1 env
      v2= eval e2 env
      Just f= H.lookup op boolOps
   in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env = 
  let v1= eval e1 env
      v2= eval e2 env
      Just f= H.lookup op compOps
   in liftCompOp f v1 v2
--- ### If Expressions

eval (IfExp e1 e2 e3) env =  
  let v1= eval e1 env   -------condition
   in case v1 of 
        BoolVal True -> eval e2 env
        BoolVal False-> eval e3 env
        _            -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application
---eval (AppExp (FunExp [](BoolExp False)) []) H.empty
eval (FunExp params body) env =  CloVal params body env

eval (AppExp e1 args) env =
    case (eval e1 env) of
        (CloVal params body clenvq) ->
            let vals = map (\x ->eval x env) args --------map (flip eval env) args
            in eval body (H.union (H.fromList(zip params vals)) clenvq) -----Q2: why union here not insert?
        otherwise -> ExnVal "Apply to non-closure"
{-
eval (AppExp e1 args) env = 
    let v1=eval e1 env
        v2=Prelude.map (\x -> eval x env) args
        in case v1 of
        CloVal nup body nuenv ->    ---expression
            vals=(zipWith (\x y -> (x,y)) nup v2
            eval body (Prelude.foldr (\x map -> H.insert (fst x) (snd x) map) nuenv vals)
        otherwise -> ExnVal "Apply to non-closure"
-}
---let fun2 = FunExp ["k"] (IntOpExp "*" (VarExp "k") (VarExp "x"))   ---e1
---env= H.fromList [("x", IntVal 3)]
---v1= eval e1 env  ---  <["k"], IntOpExp "*" (VarExp "k") (VarExp "x"), fromList [("x",3)]>
---args =[IntExp 5]  v2=[IntVal 5]
---zipWith (\x y -> (x,y)) ["a", "b"] [IntVal 5, IntVal 3]    ---[("a",5),("b",3)]
---Prelude.map (\x -> eval x env) args
{-
eval (AppExp e1 e2) env = 
    let (CloVal v body clenv)= eval e1 env
        args= Prelude.map (\x -> (eval x env)) e2   --[IntVal 110,IntVal 10]
        zipenv=zip v args
     in eval body (zipenv ++ clenv)
-}
--- ### Let Expressions

eval (LetExp pairs body) env =
    let locals=Prelude.map (\(v,e) -> (v, eval e env)) pairs
        lh=H.fromList locals
     in eval body (H.union lh env)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env
--exec (PrintStmt (IntExp 5)) H.empty H.empty   --("5",fromList [],fromList [])

--- ### Set Statements    

exec (SetStmt var e) penv env = (val, penv, env')
   where env' = H.insert var (eval e env) env 
         val  = ""     ---------not sure how to deal with val
{-      
exec (SetStmt var e) penv env = ("", penv, (H.insert var val env))
   where val = eval e env
-}
  
--fromList [("x", eval (IntExp 5) H.empty)]
--exec (SetStmt "x" (IntExp 5)) H.empty (H.fromList [("x", IntVal 6)])
--- ### Sequencing 

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = (val1++val2, penv2, env2) 
          where (val1, penv1, env1) = exec x penv env
                (val2, penv2, env2) = exec (SeqStmt xs) penv1 env1

{-
let testenv2 = H.fromList [ ("a", IntVal 42)]
exec (SeqStmt [PrintStmt (VarExp "a"), SetStmt "a" (IntExp 24), PrintStmt (VarExp "a")]) H.empty testenv2 
> ("4224", H.empty, H.insert "a" (IntVal 24) testenv2)
exec (SeqStmt [ PrintStmt (VarExp "x"), PrintStmt (IntExp 2)]) H.empty H.empty
> ("exn: No match in env2", H.empty, H.empty)
-}

--- ### If Statements
exec (IfStmt e1 s1 s2) penv env =
  let v1= eval e1 env 
   in case v1 of 
      BoolVal True -> exec s1 penv env 
      BoolVal False -> exec s2 penv env
      _            -> (val, penv, env) ---the result has to be in a tuple 
         where val = show $ ExnVal "Condition is not a Bool"
--eval (IfExp (BoolExp False) (IntExp 5) (IntExp 10)) H.empty
---why I have to put them in a parathesis
--- ### Procedure and Call Statements
--- ### ProcedureStmt String [String] Stmt
exec p@(ProcedureStmt name args body) penv env = ("", penv',env)
    where penv'=H.insert name p penv

--exec (ProcedureStmt "p" [] (PrintStmt (VarExp "x"))) H.empty H.empty
--( "", H.fromList [("p", ProcedureStmt "p" [] (PrintStmt (VarExp "x")))], H.empty)
--exec (ProcedureStmt "p" [] (PrintStmt (VarExp "x"))) H.empty testenv2 
--("", H.fromList [("p", ProcedureStsdwdszmt "p" [] (PrintStmt (VarExp "x")))], testenv2)
-- eval ( FunExp ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1))) H.empty
-- zip  ["f"] [(["x"], IntOpExp "+" (VarExp "x") (IntExp 1), fromList [])]
-- zip ["x"] [6]
exec (CallStmt name args) penv env =
    case (H.lookup name penv) of
        Just (ProcedureStmt n ps body) ->
            let vals = map (flip eval env) args
            in let newenv = H.union (H.fromList (zip ps vals)) env
                in exec body penv newenv
        Nothing -> ("Procedure "++name++" undefined", penv, env)
{-
 exec( ProcedureStmt "fog" ["f", "g", "x"] 
      (SetStmt "x" (AppExp (VarExp "f") [(AppExp (VarExp "g") [VarExp "x"])])) )
      H.empty H.empty
exec ( CallStmt "fog" [ FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2)) 
                    , FunExp ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1))
                    , IntExp 6] )
("",fromList [("fog",ProcedureStmt "fog" ["f","g","x"] (SetStmt "x" (AppExp (VarExp "f") [AppExp (VarExp "g") [VarExp "x"]])))],fromList [])
fromList []

exec ( SeqStmt [ ProcedureStmt "fog" ["f", "g", "x"] (SetStmt "x" (AppExp (VarExp "f") [(AppExp (VarExp "g") [VarExp "x"])]))
                       , CallStmt "fog" [ FunExp ["x"] (IntOpExp "*" (VarExp "x") (IntExp 2))
                       , FunExp ["x"] (IntOpExp "+" (VarExp "x") (IntExp 1))
                       , IntExp 6]
                       , PrintStmt (VarExp "x")]) H.empty H.empty
-}
{-
exec (CallStmt name args) penv env = do
  case H.lookup name penv of
    Nothing -> ("Procedure " ++ name ++ " undefined", penv,env)
    Just (ProcedureStmt _ params body) ->
          exec body
               penv 
              (Prelude.foldr (\ (k,v) e -> H.insert k (eval v env) e) env 
                  $ zip params args)
exec _ _ _ = undefined
-}

