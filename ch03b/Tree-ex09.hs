{-
    File: ch03b/Tree-ex09.hs

    Author: yomishino

    Chapter 3 (second group of exercises), Exercise 9

    Write a function that will determine the height of a tree.    
    The definition of the binary tree type is given in the book. 
-}

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

-- |Computes the height of the tree.
height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)
