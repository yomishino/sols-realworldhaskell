{-
    File: ch04b/FoldEx-ex05++10.hs

    Author: yomishino

    Chapter 4 (second half), Exercise 5 - 10

    Implement some standard library functions using list folds.
-}

module FoldEx 
    ( myConcat
    , myTakeWhile1
    , myTakeWhile2
    , myGroupBy
    , myAny
    , myCycle
    , myWords
    , myUnlines
    )
    where


--import Data.List (foldl')
import Data.Char (isSpace)


-- Question 5 & 6

-- |@myConcat@ is a re-implementation of 'concat' using 'foldr'.
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []


-- Question 7

-- |@myTakeWhile1@ is a re-implementation of 'takeWhile' using
-- explicit recursion.
myTakeWhile1 :: (a -> Bool) -> [a] -> [a]
myTakeWhile1 _ [] = []
myTakeWhile1 p (x:xs) = if p x 
                        then x : myTakeWhile1 p xs
                        else []

-- |@myTakeWhile2@ is a re-implementation of 'takeWhile' using 'foldr'.
myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 p = foldr (\x acc -> if p x then x:acc else []) [] 


-- Question 8 & 9

-- |@myGroupBy@ is a re-implementation of 'groupBy' defined in
-- `Data.List` using a fold.
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy r = foldr step []
    where step x [] = [[x]]
          step x yss@(ys:yss') = if r x (head ys)
                                 then (x:ys):yss'
                                 else [x]:yss


-- Question 10

-- |@myAny@ is a re-implementation of 'any' using a fold.
myAny :: Foldable t => (a -> Bool) -> t a -> Bool
myAny p = foldr ((||) . p) False

-- |@myCycle@ is a re-implementation of 'cycle' using a fold.
myCycle :: [a] -> [a]
myCycle [] = error "empty list"
myCycle xs = foldr (\_ acc -> acc ++ myCycle xs) xs xs

-- |@myWords@ is a re-implementation of 'words' using a fold.
myWords :: String -> [String]
myWords xs = tail $ foldr step [] (' ':xs)
    where step x acc@(ys:yss) =
            if isSpace x
            then if null ys then acc else []:acc
            else (x:ys):yss
          step x _ = if isSpace x then [[]] else [[x]]

-- |@myUnlines@ is a re-implementation of 'unlines' using a fold.
myUnlines :: [String] -> String
myUnlines = foldr (\x acc -> x ++ "\n" ++ acc) ""

