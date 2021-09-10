--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: (Num a1, Ord a1) => a1 -> [a2] -> [a2]

mytake 0 _                  = []
mytake n []                 = []
mytake n (x:xs) | n>0 = x : mytake (n-1) xs
                | n<0 = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop 0 (x:xs) = (x:xs)
mydrop n [] = []
mydrop n (x:xs) | n>0 = mydrop (n-1) xs
                | n<0 = (x:xs)

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev []=[]
rev (x:xs) = aux (x:xs) []
    where aux [] a =a
          aux (x:xs) a = aux xs (x:a)

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] y = y
app x [] = x
app (x:xs) y = x: app xs y 

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist []= []
inclist (x:xs)= x+1: inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a 
sumlist []=0
sumlist (x:xs) =x+ sumlist (xs)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)] 
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) =(x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs _ [] = []
addpairs [] _ = []
addpairs  x y = [ x+y| (x,y) <-myzip x y] 

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer] 
ones = [1,1..]

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats=[0,1..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer] 
fib = 0:1: addpairs fib (tail fib)
--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points! F
add :: Ord a => a -> [a] -> [a]
add a [] =[a]
add a x = quicksort (a:x)
    where quicksort []= []
          quicksort(x:xs) =     
              let smallerSorted = quicksort [a | a <- xs, a <x]  
                  biggerSorted = quicksort [a | a <- xs, a > x] 
              in  smallerSorted P.++ [x] P.++ biggerSorted 

--- ### union

-- don't forget to put the type declaration or you will lose points! F
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) | x==y = union xs (y:ys)
                    | x < y  = x:union xs (y:ys)
                    | x > y = y:union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points! 
keepdups :: Ord a =>[a] -> [a]
keepdups l = [d|(z,d)<- P.zip [0..] l, elem d $ P.take z l]
intersect :: Ord a => [a] -> [a] -> [a]
intersect l1 l2 = let l = l1 P.++ l2 in keepdups l

--- ### powerset

-- don't forget to put the type declaration or you will lose points! F
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs 

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union xss (map' (x:) xss)
    where xss = powerset xs

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points! F
inclist' :: Num a => [a] -> [a]
inclist'  l = P.map (+1) l

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a 
sumlist' xs = P.foldl (\acc x -> acc + x) 0 xs 

any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) = if (p x) then True else any p xs

-----is it a tail recursion?
all' p [] = True
all' p (x:xs) | p x       = all' p xs  -----p x == True
              | otherwise = False

all2 p xx = aux p xx True
  where aux p [] a =a
        aux p (x:xs) a = aux p xs (a==p x)   

report a = a
nth 0 (x:xs) = x
nth n (x:xs) = nth (n-1) xs
---kmultlist :: (Ord t, Num t, Show t) => [t] -> (t -> t) -> t
kmultlist xx k =  aux xx [k]
    where aux [] klist =trace "Base case." $ (head klist) 1
          aux (0:xs) _ = trace "Skip case."$ k 0
          aux (x:xs) klist | trace ("where we are:" ++ show x) $ x < 0 =  (nth (0-x) klist) 1
                           | trace ("Recursion case." ++ show (x,xs)) otherwise = aux xs ( (\v -> (head klist) (v * x)) : klist)

-----kmultlist [2, 3, 4, 5, 6, -2, 0, 2, 4, 6] report