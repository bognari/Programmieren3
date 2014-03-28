module Suchbaum where

-- Aufgabe 12 (binäre Suchbäume)

data Suchbaum el = Blatt | Knoten (Suchbaum el) el (Suchbaum el)

-- 12.1

istElement :: (Ord el) => el -> Suchbaum el -> Bool
istElement _ Blatt = False
istElement a (Knoten sb1 w sb2)
    | a == w = True
    | a <= w = istElement a sb1
    | a >= w = istElement a sb2

-- 12.2

suchbaumFold :: accu -> (accu -> el -> accu -> accu) -> Suchbaum el -> accu
suchbaumFold accuBlatt accuKnoten tree = fold tree where
    fold Blatt                        = accuBlatt
    fold (Knoten links wurzel rechts) = accuKnoten ( fold links) wurzel ( fold rechts)

-- Zusatz

istElement' :: (Ord el) => el -> Suchbaum el -> Bool
istElement' a tree = suchbaumFold False (\ l w r -> if l then l else if w == a then True else r) tree