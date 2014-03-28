module MyMain where

myXor :: Bool -> Bool -> Bool


nicht :: Bool -> Bool
nicht = \b -> 
						if b 
					then 
						False 
						else 
				True

myXor a b = 
				(a && nicht b) 
	|| 
				(nicht a && b)



sortList :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortList comp a = mergeSort comp a  


mergeSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergeSort comp liste = ms (split liste)
	where	
		ms [l]   = l
		ms liste = ms (mergepaare comp liste)
			
-- Hilfsfunktionen

split :: (Ord a) => [a] -> [[a]]
split []     = []
split (x:xs) = [x]:(split xs)

mergepaare :: (Ord a) => (a -> a -> Bool) -> [[a]] -> [[a]]
mergepaare comp []           = []
mergepaare comp x@[l]        = x
mergepaare comp (l1:l2:rest) = (merge comp l1 l2):(mergepaare comp rest)

merge :: (Ord a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge comp [] b = b
merge comp a [] = a
merge comp a@(x:xs) b@(y:ys) 	| comp x y  = x:(merge comp xs b)
								| otherwise = y:(merge comp a ys)