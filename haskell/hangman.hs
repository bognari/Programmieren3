module Hangman where

import Data.Char
import System.IO

w = "Hangman"
maxI = 5

hangman :: String -> Int -> IO ()
hangman cs i | i > maxI = putStrLn "\nverloren"
             | all (`elem` cs) (map toLower w) = do
                putStrLn " "
                printWord cs
                putStrLn "\ngewonnen"
hangman cs i = do
    putStrLn " "
    printWord cs
    putStrLn "\nWelcher Buchstabe?"
    c <- getChar >>= (return.toLower)
    if (c `elem` (map toLower w)) then
      hangman (c:cs) i 
    else do
      putStrLn $ "\n" ++ (show (i+1)) ++ " falsch\n"
      hangman cs (i+1)

printWord :: String -> IO ()
printWord cs = mapM_ pC w 
  where
    pC x  | x == ' ' = putChar ' '
          | toLower x `elem` cs = putChar x
          | otherwise = putChar '_'

main = do
  hSetBuffering stdin NoBuffering
  hangman " " 0