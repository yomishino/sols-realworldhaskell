{-
    File: ch04a/SafeFunc-ex01.hs

    Author: yomishino

    Chapter 4 (first half), Exercise 1

    Write "safe" definitions of the standard partial list functions.
-}

module SafeFunc (safeHead, safeTail, safeLast, safeInit) where

-- |A safe 'head' function.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

-- |A safe 'tail' function.
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

-- |A safe 'last' function.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- |A safe 'init' function.
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

