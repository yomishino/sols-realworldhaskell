{-
    File: ch03b/Intersperse-ex07+08.hs

    Author: yomishino

    Chapter 3 (second group of exercises), Exercise 7 & 8

    Define a function that joins a list of lists together 
    using a separator value.
-}

-- |Joins a list of lists into one list,
-- separating each element (the sublists) with the given separator.
-- The separator is added after each element except for the last one.
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [xs] = xs
intersperse sep (xs:xss) = xs ++ [sep] ++ intersperse sep xss
