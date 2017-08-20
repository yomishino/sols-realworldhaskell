{-
    File: ch03b/SortSub-ex06.hs

    Author: yomishino

    Chapter 3 (second group of exercises), Exercise 6

    Create a function that sorts a list of lists based 
    on the length of each sublist.
-}

import Data.List

-- |Sorts a list of lists based on the length of each sublist
-- in ascending order.
sortBySublist :: Ord a => [[a]] -> [[a]]
sortBySublist = sortOn length

