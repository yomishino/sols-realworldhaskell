{-
    File: lastButOne-ex02.hs

    Author: yomishino

    Chapter 2, Exercise 2

    Write a function that returns the element before the last.
-}


-- |Return the element before the last, that is,
-- the second last element in the list.
-- A reimplementation of 'init'.
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "list too short"
lastButOne (x:y:ys)  
    | null ys    = x
    | otherwise  = lastButOne (y:ys)

