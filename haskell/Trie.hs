-- Algorithmieren und Programmieren – Sommmersemester 2010
-- Brandenburgische Technische Universität Cottbus
--
-- Lösung zu den Haskell-Programmieraufgabe von Übungsblatt 7

module Trie (
    Trie,
    empty,
    insert,
    modify,
    delete,
    find,
    findPrefix,
    linearize
) where

data Trie a b = Node (Maybe b) [(a,Trie a b)] deriving Show

empty :: Eq a => Trie a b
empty = Node Nothing []

insert :: Eq a => [a] -> b -> Trie a b -> Trie a b
insert as b = modify as b (const b)

modify :: Eq a => [a] -> b -> (b -> b) -> Trie a b -> Trie a b
modify []      b  _  (Node Nothing   cs)  = Node (Just b)      cs
modify []      _  f  (Node (Just b)  cs)  = Node (Just (f b))  cs
modify (a:as)  b  f  (Node mb        cs)  = Node mb            (insertSubTrie cs)
    where
        insertSubTrie []           =  (a,modify as b f empty):[]
        insertSubTrie ((a',t):cs)  |  a == a'    = (a,modify as b f t):cs
                                   |  otherwise  = (a',t):insertSubTrie cs

delete :: Eq a => [a] -> Trie a b -> Trie a b
delete []      (Node _   cs)  = Node Nothing  cs
delete (a:as)  (Node mb  cs)  = Node mb       (deleteSubTrie cs)
    where
        deleteSubTrie []           =  []
        deleteSubTrie ((a',t):cs)  |  a == a'    =  case (delete as t) of
                                                        (Node Nothing [])  -> cs
                                                        t'                 -> (a',t'):cs
                                   |  otherwise  =  (a',t):deleteSubTrie cs

find :: Eq a => [a] -> Trie a b -> Maybe b
find as t = mb
    where
        (Node mb _) = findPrefix as t

findPrefix :: Eq a => [a] -> Trie a b -> Trie a b
findPrefix []      t              = t
findPrefix (a:as)  (Node _   cs)  = findSubTrie cs
    where
        findSubTrie []           =  empty
        findSubTrie ((a',t):cs)  |  a == a'    = findPrefix as t
                                 |  otherwise  = findSubTrie cs

linearize :: Eq a => Trie a b -> [b]
linearize (Node Nothing   cs)  = foldr (\(_,t) bs -> linearize t ++ bs) [] cs
linearize (Node (Just b)  cs)  = b : linearize (Node Nothing cs)

