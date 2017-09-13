{-
    File: ch04b/AsInt-ex01++04.hs

    Author: yomishino

    Chapter 4 (second half), Exercise 1 - 4

    Rewrite and improve upon the 'asInt' function.
-}

module AsInt (asInt_fold, asInt_either) where

import Data.Char
import Data.List


-- Question 1 ~ 3

-- |@asInt_fold@ converts a String into an integer.
asInt_fold :: String -> Int
asInt_fold [] = 0
asInt_fold xs@(x:xs')
     | x == '-'  = - asInt_fold' xs'
     | otherwise = asInt_fold' xs
     where asInt_fold' = foldl' next_digit 0 
           next_digit acc d = 10 * acc + digitToInt d
 

-- Question 4
 
type ErrorMessage = String
type Ei = Either ErrorMessage Int

-- |@asInt_either@ converts a String into an integer,
-- wrapped inside `Ei`.
asInt_either :: String -> Ei
asInt_either [] = Right 0
asInt_either xs@(x:xs')
    | x == '-'  = let converted = asInt_ei' xs'
                  in either Left (Right . negate) converted 
    | otherwise = asInt_ei' xs


-- |Helper for 'asInt_either'.
-- Converts a string (with the head '-', if any, stripped) 
-- into a positive integer, wrapped inside `Ei`.
asInt_ei' :: String -> Ei
asInt_ei' = foldl' next_ei (Right 0)
    where next_ei acc d =
            if isDigit d
            then either Left (next_digit d) acc
            else Left $ "non-digit '" ++ [d] ++ "'"
          next_digit d acc' = Right $ digitToInt d + 10 * acc'

