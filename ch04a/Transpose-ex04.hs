{-
    File: ch04a/Transpose-ex04.hs

    Author: yomishino

    Chapter 4 (first half), Exercise 4

    Write a program that transposes the text in a file.
-}

import System.Environment (getArgs)
import Data.List (transpose)


interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inFile outFile = do
    input <- readFile inFile
    writeFile outFile (function input)


main :: IO ()
main = mainWith transposeText 
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"


-- |Transpose a given string of text. 
-- Automatically append a line terminator @'\n'@ at the end of
-- each substring that consists of characters in the corresponding column
-- in each line of text.
--
-- For example,
--
-- >>> transposeText "hello\nworld\n"
-- "hw\neo\nlr\nll\nod\n"
-- 
transposeText :: String -> String
transposeText t = unlines (transpose lns)
    where lns = lines (toLF t)


-- |Conver text in CRLF to LF, by removing all @'\r'@ from the text.
toLF :: String -> String
toLF = filter (/= '\r')


