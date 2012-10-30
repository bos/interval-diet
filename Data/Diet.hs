--
--  Diet.hs -- Implementation of Discrete Interval Encoding Trees
--             (described in the paper "Diets for Fat Sets", JFP)
--
module Diet (Diet(..),
             member,insert,delete,
             foldDiet,size,toList
            ) where

import Data.Ix

--
-- Discrete is an extension of Ix
--
-- module Discrete (Discrete(..)) where
--
-- class Ix a => Discrete a where
--   pre, suc :: a -> a
--   adjacent :: a -> a -> Bool
--   adjacent x y = suc x==y
--
-- instance Discrete Int where
--   pre x = x-1
--   suc x = x+1



data Diet a = Empty | Node (Diet a) (a,a) (Diet a)
     deriving (Eq)


-- for a convenient debugging:
--
showsDiscrete :: (Show a) => (a,a) -> ShowS
showsDiscrete (i,j) = (" ["++) . shows i . (',':) . shows j . ("] "++)

showsDiet :: (Show a) => Diet a -> ShowS
showsDiet Empty          = id
showsDiet (Node l int r) =
          ('(':) . showsDiet l . showsDiscrete int . showsDiet r . (')':)

instance (Show a) => Show (Diet a) where
  showsPrec _ d = showsDiet d


adjacent :: (Enum a, Eq a) => a -> a -> Bool
adjacent x y = succ x == y

----------------------------------------------------------------------
-- UTILITIES
----------------------------------------------------------------------

splitMin :: Enum a => Diet a -> (Diet a,(a,a))
splitMin (Node Empty i r) = (r,i)
splitMin (Node l i r)     = (Node d i r,i') where (d,i') = splitMin l

splitMax :: Enum a => Diet a -> (Diet a,(a,a))
splitMax (Node l i Empty) = (l,i)
splitMax (Node l i r)     = (Node l i d,i') where (d,i') = splitMax r

joinLeft :: (Enum a, Eq a) => Diet a -> Diet a
joinLeft d@(Node Empty _ _) = d
joinLeft (Node l (i,j) r) | adjacent lj i  =  Node l' (li,j) r
                          | otherwise      =  Node l (i,j) r
                            where (l',(li,lj)) = splitMax l

joinRight :: (Enum a, Eq a) => Diet a -> Diet a
joinRight d@(Node _ _ Empty) = d
joinRight (Node l (i,j) r) | adjacent j ri  =  Node l (i,rj) r'
                           | otherwise      =  Node l (i,j) r
                             where (r',(ri,rj)) = splitMin r

merge :: Enum a => Diet a -> Diet a -> Diet a
merge l Empty = l
merge Empty r = r
merge l r     = Node l' i r where (l',i) = splitMax l


----------------------------------------------------------------------
-- MAIN FUNCTIONS
----------------------------------------------------------------------

member :: (Ix a) => a -> Diet a -> Bool
member x Empty = False
member x (Node l (i,j) r) | inRange (i,j) x  =  True
                          | x<i              =  member x l
                          | otherwise        =  member x r

insert :: (Enum a, Ord a) => a -> Diet a -> Diet a
insert x Empty = Node Empty (x,x) Empty
insert x d@(Node l (i,j) r) =
       if x<i then
          if adjacent x i then joinLeft (Node l (x,j) r)
                          else Node (insert x l) (i,j) r else
       if x>j then
          if adjacent j x then joinRight (Node l (i,x) r)
                          else Node l (i,j) (insert x r)
       else d

delete :: (Enum a, Ord a) => a -> Diet a -> Diet a
delete x Empty = Empty
delete x (Node l (i,j) r)
         | x<i        =  Node (delete x l) (i,j) r
         | x>j        =  Node l (i,j) (delete x r)
         | x==i       =  if i==j then merge l r else Node l (succ i,j) r
         | x==j       =  Node l (i,pred j) r
         | otherwise  =  Node l (i,pred x) (Node Empty (succ x,j) r)


----------------------------------------------------------------------
-- MORE FUNCTIONS ...
----------------------------------------------------------------------

foldDiet :: (b -> (a,a) -> b -> b) -> b -> Diet a -> b
foldDiet f u Empty        = u
foldDiet f u (Node l i r) = f (foldDiet f u l) i (foldDiet f u r)

size :: Ix a => Diet a -> Int
size = foldDiet (\x i y->x+rangeSize i+y) 0

toList :: Ix a => Diet a -> [a]
toList = foldDiet (\xs i ys->xs++range i++ys) []


----------------------------------------------------------------------
-- APPLICATION FUNCTIONS, EXAMPLES
----------------------------------------------------------------------

build :: (Enum a, Ord a) => [a] -> Diet a
build = foldl (flip insert) Empty

l = [6,9,2,13,8,14,10,7,5]
d = build l
