{-
    File: ch03b/Palindrome-ex04+05.hs

    Author: yomishino

    Chapter 3 (second group of exercises), Exercise 4 & 5

    - Turn a list into a palindrome.
    - Determine whether a list is a palindrome.
-}


-- |Builds a palindrome from the given list.
buildPalin :: [a] -> [a]
buildPalin [] = []
buildPalin (x:xs) = x:buildPalin xs ++ [x]

-- |True if the given list is a palindrome.
-- The empty list is regarded here as a palindrome,
-- although there seems no consensus reached yet.
isPalin :: Eq a => [a] -> Bool
isPalin [] = True
isPalin [x] = True
isPalin xs = head xs == last xs && 
             isPalin (init (tail xs))

