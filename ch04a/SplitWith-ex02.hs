{-
    File: ch04a/SplitWith-ex02.hs

    Author: yomishino

    Chapter 4 (first half), Exercise 2

    Write a function @splitWith@.
-}


-- |Act similarly to 'words', but take a predicate and a list of any type,
-- and splits its input list on every element for which the predicate 
-- returns @False@.
-- The function mimics the behaviour of 'words' so that 
-- >    splitWith (\= ' ') " foo bar rawr "
-- returns the same result as does
-- >    words " foo bar rawr "
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs
    | null fstPart  = splitWith p sndPart
    | otherwise     = fstPart:splitWith p sndPart
    where fstPart = takeWhile p xs
          sndPart = if null rem
                    then rem
                    else tail rem
          rem = dropWhile p xs

