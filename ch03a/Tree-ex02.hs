{-
    File: ch03a/Tree-ex02.hs

    Author: yomishino

    Chapter 3 (first group of exercises), Exercise 2

    Define a tree type that has only one constructor,
    using the Maybe type to refer to a node's children.
-}

data Tree a = Node (Maybe a)
    deriving (Show)
