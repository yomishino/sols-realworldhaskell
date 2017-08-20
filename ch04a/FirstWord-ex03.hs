{-
    File: ch04a/FirstWord-ex03.hs

    Author: yomishino

    Chapter 4 (fisrt half), Exercise 3

    Write a program that prints the first word of each line of its input.
-}

import System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inFile outFile = do
    input <- readFile inFile
    writeFile outFile (function input)


main :: IO ()
main = mainWith fstWord
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

-- |Get the first word of the given string.
fstWord :: String -> String
fstWord s = unlines (fstWords lns)
    where fstWords = map (\l -> if null l then "" else head (words l))    
          lns = lines s

