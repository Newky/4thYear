-- CS3016/CS4012 Tutorial 4  Richard Delaney 08479950 

module Tut04 where

import Data.List

data BinTree k d = Nil | Branch (BinTree k d) k d (BinTree k d) 

addEntry :: (Ord k) => k -> d -> BinTree k d -> BinTree k d
addEntry key datum Nil = Branch Nil key datum Nil
addEntry key datum (Branch ltree key' datum' rtree)
 | key < key'  =  Branch (addEntry key datum ltree) key' datum' rtree
 | key > key'  =  Branch ltree key' datum' (addEntry key datum rtree)
 | otherwise   =  Branch ltree key datum rtree

findEntry key Nil = Nothing
findEntry key (Branch ltree key' datum' rtree)
 | key < key'  =  findEntry key ltree
 | key > key'  =  findEntry key rtree
 | otherwise   =  Just datum'
 
tree2list Nil = []
tree2list (Branch ltree key datum rtree)
 = tree2list ltree ++ [(key,datum)] ++ tree2list rtree
 
list2tree :: (Ord k) => [(k,d)] -> BinTree k d
list2tree = foldr (uncurry addEntry) Nil

mergeTrees tree1 tree2 
 = list2tree (tree2list tree1 ++ tree2list tree2)

-- Q1  Write an instance of Eq for BinTree (the derived one)
--     (Leave this commented out in the final deliverable)
-- instance (Eq a, Eq b) => Eq (BinTree a b) where
-- 	Branch a w x b == Branch c y z d = w == y && x == z && a == c && b == d
-- 	Nil == Nil = True
-- 	_ == _ = False
-- 
-- Q2  Write an instance of Show for BinTree

instance (Show a, Show b) => Show (BinTree a b) where
 	show (Branch a b c d) =  "{" ++ show' a ++ show b ++ " |-> " ++ show c ++ ", " ++ show' d ++ "}"
	show _ = []

show' (Branch a b c d) = show' a ++ show b ++ " |-> " ++ show c ++ ", " ++ show' d
show' _ = []

-- Q3 Write an instance of Eq for BinTree that respects Show
--
instance (Eq a, Eq b) => Eq (BinTree a b) where
	a == b = tree2list a == tree2list b

-- Q4 Design a Ordered Collection Class (OrdColl): none fuse
class OrdColl a where 
	fuse:: a->a->a
	none:: a

-- Q5 Instantiate List as OrdColl

instance (Ord a) => OrdColl [a] where
	fuse a b = qsort $ a ++ b
	none = []


qsort (x:xs) = (qsort left) ++ [x] ++ (qsort right)
	       where
		left = [ a | a <- xs, a <= x]
		right = [ a | a <- xs, a > x]
qsort [] = []

-- Q6 Instantiate BinTRee as OrdColl
 
instance (Ord a, Ord b) => OrdColl (BinTree a b) where
	fuse a b = list2tree . qsort $ tree2list a ++ tree2list b
	none  = Nil
  
