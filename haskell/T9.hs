-- Algorithmieren und Programmieren – Sommmersemester 2010
-- Brandenburgische Technische Universität Cottbus
--
-- Lösung zu den Haskell-Programmieraufgabe von Übungsblatt 7

module T9 (
    T9Char (..),
    T9String,
    fromDigits,
    fromString,
    fromChar,
    stringToT9Number,
    T9Trie,
    generate,
    find
) where

import Data.Char (toLower)
import qualified Trie
import Top10000

t9dict = generate top10000de

data T9Char  =  T9_SPACE  | T9_INVALID
             |  T9_abc    | T9_def  | T9_ghi
             |  T9_jkl    | T9_mno  | T9_pqrs
             |  T9_tuv    | T9_wxyz
    deriving (Enum, Eq, Show)

type T9String = [T9Char]

fromString :: String -> T9String
fromString = map fromChar

fromChar :: Char -> T9Char
fromChar c  |  c' `elem` "aäbc"   = T9_abc
            |  c' `elem` "def"    = T9_def
            |  c' `elem` "ghi"    = T9_ghi
            |  c' `elem` "jkl"    = T9_jkl
            |  c' `elem` "mnoö"   = T9_mno
            |  c' `elem` "pqrsß"  = T9_pqrs
            |  c' `elem` "tuüv"   = T9_tuv
            |  c' `elem` "wxyz"   = T9_wxyz
            |  c' == ' '          = T9_SPACE
            |  otherwise          = T9_INVALID
    where c' = toLower c

fromDigits :: [Int] -> T9String
fromDigits = map fromDigit

fromDigit :: Int -> T9Char
fromDigit c  |  c == 2 = T9_abc
             |  c == 3 = T9_def
             |  c == 4 = T9_ghi
             |  c == 5 = T9_jkl
             |  c == 6 = T9_mno
             |  c == 7 = T9_pqrs
             |  c == 8 = T9_tuv
             |  c == 9 = T9_wxyz
             |  c == 0 = T9_SPACE
             |  otherwise = T9_INVALID


stringToT9Number :: String -> [Int]
stringToT9Number = map fromEnum . fromString

type T9Trie = Trie.Trie T9Char [String]

generate :: [String] -> T9Trie
generate = foldr (\s -> Trie.modify (fromString s) [s] (s:)) Trie.empty

find :: T9String -> T9Trie -> [String]
find as t =  case Trie.find as t of
                 Nothing  -> []
                 Just bs  -> bs