{-
    File: ch03b/Mean-ex03.hs

    Author: yomishino

    Chapter 3 (second group of exercises), Exercise 3

    Write a function that computes the mean of a list.
-}

-- |Computes the mean of the list.
mean :: Fractional a => [a] -> a
mean [] = error "empty list"
mean xs = sum xs / fromIntegral (length xs)

