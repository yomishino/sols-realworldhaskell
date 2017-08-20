{-
    File: ch01/WC-ex04.hs

    Author: yomishino

    Chapter 1, Exercise 4
    
    Count the number of characters in a file.
-}

main = interact count
    where count input = show (length input) ++ "\n"

