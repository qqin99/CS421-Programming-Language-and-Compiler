{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs)| n<=0 = []
               | n>0 = x: mytake (n-1) xs
--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop n (x:xs) | n<=0 = (x:xs)
                | n>0  = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev []= []
rev (x:xs) = aux (x:xs) []
    where aux [] a = a
          aux (x:xs) a =aux xs (x:a)
--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] y= y
app x []=x
app (x:xs) y = x: (app xs y)

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist []=[]
inclist (x:xs) = (x+1): inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (x:xs)= x+ sumlist xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip x [] = []
myzip [] x = []
myzip (x:xs) (y:ys) = (x,y) : (myzip xs ys)

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs x [] = []
addpairs [] x = []
addpairs x y = [x+y|(x,y)<-myzip x y]

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = [1,1..]

--- ### nats
nats :: [Integer]
nats = [0,1..]
-- don't forget to put the type declaration or you will lose points!


--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0:1:(addpairs fib  (P.tail fib))

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n []= n:[]
add n (x:xs) | n==x = x:xs 
             | n<x= n:x:xs
             | n>x = x : add n xs
--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] []= []
union [] y = y
union x [] = x
union (x:xs) (y:ys) |x==y = x: union xs ys
                    |x>y = y: union (x:xs) ys
                    |x<y = x : union xs (y:ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] x= []
intersect x []= []
intersect (x:xs) (y:ys) | x==y= x: (intersect xs ys)
                        | x<y = intersect xs (y:ys)
                        | x>y = intersect (x:xs) ys


--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset = undefined

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.foldr(\x acc->(x+1):acc) []

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldl (\acc x->x+acc) 0