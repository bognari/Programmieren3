module Aufgabenblatt2 where

-- Aufgabe 1 (Mengen als Listen)
testListe1 = [1,2,4,6,8,10]

testListe2 = [3,6,9,12,15]

istElement :: (Ord a) => a -> [a] -> Bool
istElement _ [] = False
istElement a (x:xs) | (a == x) = True
                    | (a < x)  = False
                    | otherwise = istElement a xs
					
istTeilmenge :: (Ord a) => [a] -> [a] -> Bool
istTeilmenge [] _ 		= True
istTeilmenge _ [] 		= False
istTeilmenge a@(x:xs) (y:ys)  | x == y  = istTeilmenge xs ys
                              | x > y   = istTeilmenge a ys
                              | otherwise = False

vereinigung :: (Ord a) => [a] -> [a] -> [a]
vereinigung [] ys = ys
vereinigung xs [] = xs
vereinigung a@(x:xs) b@(y:ys) 	| x <  y = x : vereinigung xs b
                                | x == y = x : vereinigung xs ys
                                | otherwise = y : vereinigung a ys

schnitt :: (Ord a) => [a] -> [a] -> [a]
schnitt _ [] = []
schnitt [] _ = []
schnitt a@(x:xs) b@(y:ys)	    | x == y     = x : schnitt xs ys
                              | x < y     = schnitt xs b
                              | otherwise = schnitt a ys
                  
-- Aufgabe 2
funkTest = [(-2, 4), (-1, 1), (0, 0), (1, 1), (2, 4)]

result :: (Ord a) => [(a,b)] -> a -> b
result [] _ = error "undefiniert"
result ((k,v):xs) a  | a == k = v
                     | a >  k = result xs a
                     | otherwise = error "undefiniert"
                     
evtlResultat :: (Ord a) => [(a,b)] -> a -> (Maybe b)
evtlResultat [] _ = Nothing
evtlResultat ((k,v):xs) a  | a == k = Just v
                           | a >  k = evtlResultat xs a
                           | otherwise = Nothing
                           
urbilder :: (Ord a) => [(a,a)] -> a -> [a]
urbilder [] _= []
urbilder ((k,v):xs) a | a == v = k : urbilder xs a
                      | otherwise = urbilder xs a

echteArgumente :: (Ord a) => [(a,b)] -> [a]
echteArgumente [] = []
echteArgumente ((k,v):xs) = vereinigung [k] $ echteArgumente xs

fixPunkte :: (Ord a) => [(a,a)] -> [a]
fixPunkte [] = []
fixPunkte ((k,v):xs) | k == v = vereinigung [k] $ fixPunkte xs 
                     | otherwise = fixPunkte xs
                     
--funKomposition :: (Ord a, Ord b, Ord c) => [(b,c)] -> [(a,b)] -> [(a,c)]
funKomposition :: (Ord a, Ord b, Ord c) => [(b,c)] -> [(a,b)] -> [(a,c)]
funKomposition [] _ = []
funKomposition _ [] = []
funKomposition o ((k,v):is) = case evtlResultat o v of 
  Nothing  -> funKomposition o is
  (Just a) -> vereinigung [(k, a)]  (funKomposition o is)
--funKomposition o ((k,v):is) = vereinigung (map (\t -> (k,(snd t))) 
--(filter (\t -> (fst t) == v) o)) (funKomposition o is)

-- Aufgabe 3
testRel1 = [(4,-2),(1,-1),(0,0),(1,1),(4,2)]
testRel2 = [(0,1),(1,2)]

relKomposition :: (Eq b) => [(b,c)] -> [(a,b)] -> [(a,c)]
relKomposition o i = [(a,c)| (a,b1)  <- i, (b2, c) <- o, b1 == b2] 

-- Aufgabe 4 Instanziierung von Typklassen

data Ausdruck = Konstante Integer
  | Variable String
  | Summe Ausdruck Ausdruck
  | Differenz Ausdruck Ausdruck
  | Produkt Ausdruck Ausdruck
  
instance Eq Ausdruck where
	(==) (Konstante z)      (Konstante z')      = z == z'
	(==) (Variable b)       (Variable b')       = b == b'
	(==) (Summe a1 a2)      (Summe a1' a2')     = a1 == a1' && a2 == a2'
	(==) (Differenz a1 a2)  (Differenz a1' a2') = a1 == a1' && a2 == a2'
	(==) (Produkt a1 a2)    (Produkt a1' a2')   = a1 == a1' && a2 == a2'
	(==) _                  _                   = False

instance Show Ausdruck where
	show (Konstante z)      = "(Konstante "   ++ show z ++ ")"
	show (Variable b)       = "(Variable \""  ++ b ++ "\")"
	show (Summe a1 a2)      = "(Summe "       ++ show a1 ++ " " ++ show a2 ++ ")"
	show (Differenz a1 a2)  = "(Differenz "   ++ show a1 ++ " " ++ show a2 ++ ")"
	show (Produkt a1 a2)    = "(Produkt "     ++ show a1 ++ " " ++ show a2 ++ ")"

-- Aufgabe 5 eigene Klasse

class Spiegelbar val where
  gespiegelt :: val -> val
  
instance Spiegelbar [a] where
  gespiegelt = reverse
  -- gespiegelt [] = []
  -- gespiegelt (x:xs) = gespiegelt xs ++ [x]
  
  
data Baum val = Blatt val
              | Verzweigung (Baum val) (Baum val) 
              deriving (Show)

instance Spiegelbar (Baum val) where
  gespiegelt (Blatt a)            = Blatt a
  gespiegelt (Verzweigung t1 t2)  = Verzweigung (gespiegelt t2) (gespiegelt t1)
  
-- Aufgabe 6 Babylonisches Wurzelziehen

wurzel :: Double -> Double -> Double -> Double -> Double
wurzel e x y z0 | abs (z0 - z1) < e = z1
                | otherwise         = wurzel e x y z1
                  where
                    z1 = ((x - 1)*(z0 ** x) + y) / (x * (z0 ** (x - 1)))

-- Aufgabe 7
data Menge a = Menge [a]

leer :: Menge a
leer = Menge []

-- 7.1

einfuegen :: (Ord a) => a -> Menge a -> Menge a
einfuegen a (Menge xs) = Menge $ einfuegen' a xs
  where
    einfuegen' :: (Ord a) => a -> [a] -> [a]
    einfuegen' a [] = [a]
    einfuegen' a l@(x:xs) | a < x     = a : l
                          | a == x    = l
                          | otherwise = x : einfuegen' a xs
                          
loeschen :: (Ord a) => a -> Menge a -> Menge a
loeschen a (Menge xs) = Menge $ loeschen' a xs
  where
    loeschen' :: (Ord a) => a -> [a] -> [a]
    loeschen' _ [] = []
    loeschen' a l@(x:xs) | a == x     = xs
                         | a < x      = l
                         | otherwise  = x : loeschen' a xs 
                         
vereinigungM :: (Ord a) => Menge a -> Menge a -> Menge a
vereinigungM (Menge xs) (Menge ys) = Menge $ vereinigungM' xs ys
  where 
    vereinigungM' :: (Ord a) => [a] -> [a] -> [a]
    vereinigungM' [] ys = ys
    vereinigungM' xs [] = xs
    vereinigungM' a@(x:xs) b@(y:ys) 	| x <  y    = x : vereinigungM' xs b
                                      | x == y    = x : vereinigungM' xs ys
                                      | otherwise = y : vereinigungM' a ys

schnittM :: (Ord a) => Menge a -> Menge a -> Menge a
schnittM (Menge xs) (Menge ys) = Menge $ schnittM' xs ys
  where 
    schnittM' :: (Ord a) => [a] -> [a] -> [a]
    schnittM' _ [] = []
    schnittM' [] _ = []
    schnittM' a@(x:xs) b@(y:ys)	| x == y    = x : schnittM' xs ys
                                | x < y     = schnittM' xs b
                                | otherwise = schnittM' a ys

difference :: (Ord a) => Menge a -> Menge a -> Menge a
difference (Menge xs) (Menge ys) = Menge $ difference' xs ys
  where
    difference' :: (Ord a) => [a] -> [a] -> [a]
    difference' xs [] = xs
    difference' [] _ = []
    difference' a@(x:xs) b@(y:ys) | x == y    = difference' xs ys
                                  | x < y     = x : difference' xs b
                                  | otherwise = difference' a ys

istLeer :: Menge a -> Bool
istLeer (Menge [])  = False
istLeer _           = True
                                  
istElementM :: (Ord a) => a -> Menge a -> Bool
istElementM a (Menge xs) = istElementM' a xs
  where
    istElementM' :: (Ord a) => a -> [a] -> Bool
    istElementM' - [] = False
    istElementM' a (x:xs) | (a == x)  = True
                          | (a < x)   = False
                          | otherwise = istElementM' a xs
    

istTeilmengeM :: (Ord a) => Menge a -> Menge a -> Bool
istTeilmengeM (Menge xs) (Menge ys) = istTeilmengeM' xs ys
  where 
    istTeilmengeM' :: (Ord a) => [a] -> [a] -> Bool
    istTeilmengeM' [] _ 		= True
    istTeilmengeM' _ [] 		= False
    istTeilmengeM' a@(x:xs) (y:ys)  | x == y    = istTeilmengeM' xs ys
                                    | x > y     = istTeilmengeM' a ys
                                    | otherwise = False

istEchteTeilmenge :: (Ord a) => Menge a -> Menge a -> Bool
istEchteTeilmenge (Menge xs) (Menge ys) = uncurry (&&) $istEchteTeilmenge' False xs ys
  where 
    istEchteTeilmenge' :: (Ord a) => Bool -> [a] -> [a] -> (Bool, Bool)
    istEchteTeilmenge' v [] _  = (v , True)
    istEchteTeilmenge' _  _ [] = (False , False)
    istEchteTeilmenge' v a@(x:xs) (y:ys)  | x == y    = istEchteTeilmenge' v xs ys
                                          | x > y     = istEchteTeilmenge' True a ys
                                          | otherwise = (False ,False)

minimalesElement :: Menge a -> Maybe a
minimalesElement (Menge [])    = Nothing
minimalesElement (Menge (x:_)) = Just x

maximalesElement :: Menge a -> Maybe a
maximalesElement (Menge []) = Nothing
maximalesElement (Menge xs) = Just $ last xs 

-- 7.2

instance (Show a) => Show (Menge a) where
  show (Menge xs) = "{" ++ show' xs ++ "}"
    where
      show' [] = " "
      show' [x] = show x
      show' (x:xs) = show x ++ ", " ++ show' xs
      
-- Aufgabe 8 Funktionen hoeherer Ordnung

-- 8.1
summe :: (Num zahl) => [zahl] -> zahl
summe xs = foldr (+) 0 xs

-- 8.2
produkte :: (Num zahl) => [zahl] -> [zahl] -> [zahl]
produkte xs ys = map (uncurry (*)) $zip xs ys

produkte' :: (Num zahl) => [zahl] -> [zahl] -> [zahl]
produkte' xs ys = zipWith (*) xs ys

-- 8.3

type Vektor koord = [koord]

skalarProdukt :: (Num koord) => Vektor koord -> Vektor koord -> koord
skalarProdukt xs ys = summe $ produkte xs ys

-- 8.4
type Matrix koord = [Vektor koord]

skalarMul :: (Num koord) => koord -> Vektor koord -> Vektor koord
skalarMul s v = map (*s) v 

plusVek :: (Num koord) => Vektor koord -> Vektor koord -> Vektor koord
plusVek v1 v2 = zipWith (+) v1 v2

transMatrix :: (Num koord) => Matrix koord -> Matrix koord
transMatrix ([]:_) = []
transMatrix x = (map head x) : transMatrix (map tail x)

lineareAbb :: (Num koord) => Matrix koord -> (Vektor koord -> Vektor koord)
lineareAbb m v = foldr (\v vs -> summe v : vs) [] (map (\x -> produkte' v x) (transMatrix m))

-- Aufgabe 9 Falten von Listen

-- 9.1
-- ++
pp :: [a] -> [a] -> [a]
pp xs ys = foldr (:) ys xs

concat' :: [[a]] -> [a]
concat' xs = foldr (\xs ys -> foldr (:) ys xs) [] xs

length' :: [a] -> Int
length' xs = foldr (\_ i -> succ i) 0 xs 

filter' :: (a -> Bool) -> [a] -> [a] 
filter' f xs = foldr (\a xs -> if f a then a:xs else xs) [] xs

-- 9.2
pp' :: [a] -> [a] -> [a]
--pp' xs ys = foldl (\t h -> h:t) ys $ foldl (\t h -> h:t) [] xs
pp' xs ys = foldl (\t h -> h:t) ys $ rev' xs

rev' :: [a] -> [a]
rev' xs = foldl (\t h -> h:t) [] xs

concat'' :: [[a]] -> [a]
--concat'' xs = foldl (\xs ys -> foldl (\t h -> h:t) ys $ foldl (\t h -> h:t) [] xs) [] xs 
concat'' xs = foldl (pp') [] xs

-- 10 Varianten von map und foldr fuer Baeume

-- 10.1
testBaum = ((Blatt 2 `Verzweigung` Blatt 3) `Verzweigung` Blatt 5)

baumMap :: (a -> a') -> Baum a -> Baum a'
baumMap f (Blatt v)           = Blatt $f v
baumMap f (Verzweigung b1 b2) = Verzweigung (baumMap f b1) (baumMap f b2)

baumFold ::(a -> a') -> (a' -> a' -> a') -> Baum a -> a'
baumFold f t (Blatt v) = f v
baumFold f t (Verzweigung b1 b2) = t (baumFold f t b1) (baumFold f t b2)

-- 10.2

hoehe :: Baum a -> Int
hoehe b = baumFold (\_ -> 0) (\a b -> if a <= b then a + 1 else b + 1) b

-- Aufgabe 11 Sortieren mit variabler Ordnung

-- 11. 2
mergeSort :: (a -> a -> Bool) ->[a] -> [a]
mergeSort _ [] 	= []
mergeSort f q 	= dc 	(\x -> length x <= 1) id splitMS (merge f) q

merge :: (a -> a -> Bool) ->([a],[a]) ->  [a]
merge f ([],w) 	= w
merge f (q,[]) 	= q
merge f (q@(x:xs), w@(y:ys))  | f x y = x:merge f (xs,w)
                              | otherwise = y:merge f (q,ys)

dc :: ([a] -> Bool) -> ([a] -> [b]) -> ([a] -> ([a],[a])) -> (([b],[b]) -> [b]) -> [a] -> [b]
dc simple solve split combine q | simple q 	= solve q
                                | otherwise = combine (l1,l2)
                                where 
                                  (p1,p2) = split q
                                  l1 = (dc simple solve split combine p1)
                                  l2 = (dc simple solve split combine p2)			

splitMS :: [a] -> ([a],[a])
splitMS q = splitAt (length q `quot` 2) q
			
-- 11.3
-- Datentyp zur Repraesentation von Buechern
data Buch = Buch {
                autor   :: String,
                titel   :: String,
                jahr    :: Integer,
                verlag  :: String
            } deriving (Show)

autorVergleich :: Buch -> Buch -> Bool
autorVergleich (Buch a1 _ _ _) (Buch a2 _ _ _) = a1 <= a2 

titelVergleich :: Buch -> Buch -> Bool
titelVergleich (Buch _ t1 _ _) (Buch _ t2 _ _) = t1 <= t2

jahrVergleich :: Buch -> Buch -> Bool
jahrVergleich (Buch _ _ j1 _) (Buch _ _ j2 _) = j1 <= j2

und :: (a -> a -> Bool) -> (a -> a -> Bool) -> (a -> a -> Bool)
und c1 c2 b1 b2 | t1 == t2 = c2 b1 b2
                | otherwise = t1
                  where 
                    t1 = c1 b1 b2
                    t2 = c1 b2 b1
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    