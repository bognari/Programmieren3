type Set = Integer -> Bool

myElem :: Set -> Integer  -> Bool
myElem    s    e          = s e

mySingleSet :: Integer    -> Set
mySingleSet e             = (==) e

myBoolSet :: (Integer -> Bool) -> Set
myBoolSet p = p

myListSet :: [Integer] -> Set
myListSet list =  \x -> myFind list x
    where
        myFind :: [Integer] -> Integer -> Bool
        myFind []     _ = False
        myFind (y:ys) x | x == y    = True
                        | otherwise = myFind ys x

myAdd :: Set -> Integer -> Set
myAdd s i = (mySingleSet i) `myUnion` s

myUnion :: Set -> Set -> Set
myUnion a b = \x -> ((myElem a x) || (myElem b x))

myIntersect :: Set -> Set -> Set
myIntersect a b = \x -> ((myElem a x) && (myElem b x))

myDiff :: Set -> Set -> Set
myDiff a b = \x -> ((myElem a x) && not (myElem b x))

myFilter :: Set -> (Integer -> Bool) -> Set
myFilter s f = \x -> (f x && s x)

myPrint :: Set -> Integer -> Integer -> [Integer]
myPrint s min max | min > max    = []
                  | myElem s min = min : (myPrint s (min + 1) max)
                  | otherwise    = myPrint s (min + 1) max

myForAll :: Set -> (Integer -> Bool) -> Integer -> Integer -> Bool
myForAll s p min max | min > max    = True
                     | myElem s min = p min && myForAll s p (min + 1) max
                     | otherwise    = myForAll s p (min + 1) max

myExists :: Set -> (Integer -> Bool) -> Integer -> Integer -> Bool
myExists s p min max = not (myForAll s (not.p) min max)

mySubset :: Set -> Set -> Bool
mySubset a b = myForAll a (\x -> myExists b (\y -> x == y) (-100) 100) (-100) 100 

myEquals :: Set -> Set -> Bool