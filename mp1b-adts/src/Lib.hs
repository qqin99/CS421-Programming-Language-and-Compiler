--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = (Cons x (list2cons xs))

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil =[]
cons2list (Cons n x) = n : cons2list x

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp i) = i
eval (PlusExp [])= 0
eval (PlusExp (x:xs)) = eval x + eval (PlusExp xs)
eval (MultExp [])= 1
eval (MultExp (x:xs))= eval x* eval(MultExp xs)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' xs = foldr (\x  acc-> Cons x (acc)) Nil xs

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
              |Leaf
    deriving Show

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node a left right) = a + sumTree left + sumTree right

--- ### SimpVal

-- SimpVal
data SimpVal= IntVal Integer
            | BoolVal Bool
            | StrVal String
            | ExnVal String
    deriving Show
--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp f (IntVal x) (IntVal y) =IntVal$ f  x y
liftIntOp f _ _ = ExnVal  "not an IntVal!"


removedups l = [d|(z,d)<- zip [0..] l,notElem d $ take z l]
union xs [] = xs  ----here we should use xs because that is a base case so the recursion would stop
union [] ys = ys
union (x:xs) (y:ys) | x==y = union xs (y:ys)  ---remove dup
                    | x < y  = x:union xs (y:ys)  ---sort
                    | x > y = y:union (x:xs) ys  ---sort
