module Aufgabenblatt1 where

-- Aufgabe 1 
-- 1.1, 1.2

g :: Double
g = 9.81

-- berechnet die Fallstrecke in m/s
fallstrecke :: Double -> Double
fallstrecke t = g * t**2 / 2

-- 1.3
fallstrecke' :: Double -> Double
fallstrecke' t = g' * t**2 / 2
	where
		g' = 9.81

fallstrecke'' :: Double -> Double
fallstrecke'' t = 	let g' = 9.81
					in g' * t**2 / 2

-- 1.4
fallstrecke''' :: Double -> Double
fallstrecke''' t = if t < 0 then error "negative Zeit"
                            else fallstrecke t

fallstrecke'''' :: Double -> Double
fallstrecke'''' t 	| t < 0 = error "negative Zeit"
                    | otherwise = fallstrecke t

-- Aufgabe 2          
-- 2.1 und 2.2
-- radiusNachOberflaeche berechnet aus einem Radius
-- die Oberflaeche der zugehörigen Kugel
radiusNachOberflaeche :: Double -> Double
radiusNachOberflaeche r | r < 0 = error "negativer Wert"
                        | otherwise = 4 * pi * r 

-- OberflaecheNachRadius berechnet aus der Oberflaeche
-- den Radius der zugehörigen Kugel 
oberflaecheNachRadius :: Double -> Double
oberflaecheNachRadius a | a < 0 = error "negativer Wert"
                        | otherwise = a / (4 * pi)

-- RadiusNachVolumen berechnet aus einem Radius
-- das Volumen der zugehörigen Kugel
radiusNachVolumen :: Double -> Double
radiusNachVolumen r | r < 0 = error "negativer Wert"
                    | otherwise = (4/3) * pi * r**3

-- VolumenNachRadius berechnet aus dem Volumen
-- den Radius der zugehörigen Kugel
volumenNachRadius :: Double -> Double
volumenNachRadius v | v < 0 = error "negativer Wert"
                    | otherwise = (v / (4/3 * pi))**(1/3)

-- VolumenNachOberflaeche berechnet aus dem Volumen
-- die Oberflaeche der zugehörigen Kugel
oberflaecheNachVolumen :: Double -> Double
oberflaecheNachVolumen a 	| a < 0 = error "negativer Wert"
                          | otherwise = (radiusNachVolumen.oberflaecheNachRadius) a 
                          --((a/(4 * pi))**3)  * 4/3 * pi

-- OberflaecheNachVolumen berechnet aus der Oberfläche
-- das Volumen der zugehörigen Kugel
volumenNachOberflaeche :: Double -> Double
volumenNachOberflaeche v 	| v < 0 = error "negativer Wert"
                          | otherwise = (radiusNachOberflaeche.volumenNachRadius) v
                          --((v / (4/3 * pi))**(1/3)) * 4 * pi
	
-- Aufgabe 3 (bedingte Asdrücke)
-- if-then-else
schaltjahr :: Int -> Bool
schaltjahr j = 	if (j >= 1582) 
				then 	if (mod j 400 == 0) 
						then True
						else 	if (mod j 100 == 0)
								then False
								else 	if (mod j 4 == 0)
										then True
										else False
				else False
        
-- mit guards
schaltjahr' :: Int -> Bool
schaltjahr' j 	| j < 1582 = False
                | mod j 400 == 0 = True
                | mod j 100 == 0 = False
                | mod j 4 == 0 = True
                | otherwise = False

-- als boolescher Ausdruck
schaltjahr'' j = ((j > 1582) && ((j `mod` 4) == 0) && (((j `mod` 100) /= 0) || ((j `mod` 400) == 0))) 


-- Aufgabe 4 (lokale Deklarationen)

-- 4.1

abcFormel :: (Double, Double, Double) -> (Double, Double)
abcFormel (a, b, c) = ((-b + r) / (2 * a), (-b - r) / (2 * a))
					where
						r = (b**2 - 4 * a * c) ** (1/2)

abcFormel' :: (Double, Double, Double) -> (Double, Double)
abcFormel' (a, b, c) =  let
              r = (b**2 - 4 * a * c) ** (1/2)
						in ((-b + r) / (2 * a), (-b - r) / (2 * a))
					

-- 4.2
wochentag :: Integer -> Integer -> Integer -> String
wochentag tag monat jahr = wochentage !! fromInteger (robertson tag monat jahr) 
	where
		wochentage :: [String]
		wochentage = ["Sonntag" ,"Montag" ,"Dienstag" ,"Mittwoch" ,"Donnerstag" ,"Freitag" ,"Samstag"]
		robertson :: Integer -> Integer -> Integer -> Integer
		robertson t m j = (d + t + 77 + e + (b `quot` 400) - 2 * (b `quot` 100)) `mod` 7	
			where
				a = m + 10
				b =((m - 14) `quot` 12) + j
				c = a - 12 * (a `quot` 13)
				d = (13 * c - 1)  `quot` 5
				e = 5 * (b `mod` 100) `quot` 4


-- Aufgabe 5 (logische Operatoren, Infix-Notation)

und :: Bool -> Bool -> Bool
und = (&&)

nicht :: Bool -> Bool
nicht = not

oder :: Bool -> Bool -> Bool
oder a b = nicht $ nicht a `und` nicht b

folgt :: Bool -> Bool -> Bool
folgt a b = nicht $ a `und` nicht b

gdw :: Bool -> Bool -> Bool
gdw a b = nicht (nicht (a `und` nicht b) `und` (nicht a `und` b))

xoder :: Bool -> Bool -> Bool
xoder a b = nicht (nicht a `und` nicht b) `und` nicht (a `und` b)



-- Aufgabe 6 (Rekursion über den natürlichen Zahlen)

plus :: Integer -> Integer -> Integer
plus 0 b = b
plus a b = succ (plus (pred a) b) -- gleicht plus (pred a) (succ b)
                                  -- jedoch wird in diesem Fall anders "optimiert"

mal :: Integer -> Integer -> Integer
mal 0 _ = 0
mal _ 0 = 0
mal 1 b = b
mal a b = b `plus` ((pred a) `mal` b)

hoch :: Integer -> Integer -> Integer
hoch _ 0 = 1
hoch 0 _ = 0
hoch a 1 = a
hoch a b = a `mal` (a `hoch` (pred b))


-- meta ist die 3 argumentige Ackermannfunktion
-- statt (a, b, i) hier (i, a, b)
meta :: Integer -> Integer -> Integer -> Integer
meta 0 a b = plus a b
meta 1 a b = mal a b
meta 2 a b = hoch a b 
meta _ _ 0 = 1
meta i a b = meta (pred i) a (meta i a (pred b))
-- meta 0 a b = plus a b
-- meta 1 a b = mal a b
-- meta 2 a b = hoch a b
-- meta _ _ 0 = 1
-- meta i a b = meta (pred i) a (meta i a (pred b))

-- Aufgabe 7 Datentypen
--7.1
data Ampelfarbe = Rot | Gelb | Gruen deriving(Show)
data Menge = Stueck Integer | Kilogramm Double | Liter Double deriving(Show)
data HausOderPostfach = 
    Haus {
      strasse :: String,
      hausNummer :: Integer
    } 
  | Postfach {
      postfachNummer :: Integer
    } 
  deriving(Show)
data Adresse = Adresse {
      hausOderPostfach :: HausOderPostfach,
      ort :: String,
      plz :: Integer
    } 
  deriving(Show)

{-
Konstruktoren:
Rot :: Ampelfarbe
Gelb :: Ampelfarbe
Gruen :: Ampelfarbe

Stueck :: Integer -> Menge
Kilogramm :: Double -> Menge
Liter -> Double

Haus :: String -> integer -> HausOderPostfach
Postfach :: Integer -> HausOderPostfach

Adresse :: HausOderPostfach -> String -> Integer -> Adresse

Selektoren:
strasse :: HausOderPostfach -> String
hausNummer :: HausOderPostfach -> Integer
postfachNummer :: HausOderPostfach -> Integer

hausOderPostfach :: Adresse -> HausOderPostfach
ort :: Adresse -> String
plz :: Adresse -> Integer

hausOderPostfach (Adresse (Haus "Klaus-Str." 2) "Irgendwo" 0123456)
> Haus (strasse = "Klaus-Str.", hausNummer = 2)

strasse $ hausOderPostfach (Adresse (Haus "Klaus-Str." 2) "Irgendwo" 0123456)
> "Klaus-Str."

postfachNummer $ hausOderPostfach (Adresse (Haus "Klaus-Str." 2) "Irgendwo" 0123456)
> *** Exception
-}

-- 7.2
data Menge' = Stueck' {anzahl :: Integer} 
            | Kilogramm' {gewicht :: Double}
            | Liter' {volumen :: Double}
            deriving(Show)

-- 7.3

data HausOderPostfach'  = Haus' String Integer
                        | Postfach' Integer
                        deriving(Show)
data Adresse' = Adresse' HausOderPostfach String Integer deriving(Show)

-- 7.4
strasseVonAdresse :: Adresse -> String
strasseVonAdresse = strasse.hausOderPostfach


postfachNummerVonAdresse :: Adresse -> Integer
postfachNummerVonAdresse = postfachNummer.hausOderPostfach

-- Aufgabe 8 Pattern-Matching
data SI = SI String Integer deriving(Show)
data SIListe = Leer | NichtLeer SI SIListe deriving(Show)
data VielleichtSI = Nicht | Doch SI deriving(Show)

-- 8.1
loesche :: SI -> SIListe -> SIListe
loesche a Leer = error "nicht da"
loesche (SI b c) (NichtLeer (SI d e) f ) 
            | b == d && c == e = f
            | otherwise = NichtLeer (SI d e) (loesche (SI b c) f )

ersetze :: SI -> SIListe -> SIListe
ersetze g Leer = Leer
ersetze h@(SI i _) (NichtLeer j@(SI k _) l) = 
               if i == k then NichtLeer h (ersetze h l)
                         else NichtLeer j (ersetze h l)
                                                        
findeAnIdx :: Integer -> SIListe -> VielleichtSI
findeAnIdx _ Leer                        = Nicht
findeAnIdx m (NichtLeer n o) | m == 0    = Doch n
                             | otherwise = findeAnIdx ( pred m) o

{-
a	SI
b	String
c	Integer
d	String
e	Integer
f	SIListe
g	SI
h	SI
i	String
j	SI
k	String
l	SIListe
m	Integer
n	SI
o	SIListe
-}

--8.2

liste1 = NichtLeer (SI "Bert" 7) (NichtLeer (SI "Bianca" 9) (NichtLeer (SI "Bert" 7) Leer))
liste2 = NichtLeer (SI "Bert" 8) (NichtLeer (SI "Robert" 7) Leer)
liste3 = NichtLeer (SI "Robert" 8) (NichtLeer (SI "Robert" 7) Leer)

-- a)
aufgabe8a = loesche (SI "Bert" 7) liste1
{-	
Nichtleer (SI "Bianca" 9)(Nichtleer (SI "Bert" 7) Leer)
-nur der erste Eintrag mit Uebereinstimmung wird geloescht
-}	

--b)	
aufgabe8b = ersetze (SI "Robert" 9) liste2
{-
Nichtleer (SI "Bert" 8) (Nichtleer (SI "Robert" 9) Leer)
- Ueberschreibung des Integer-Eintrags bei passendem String-Eintrag
- Ueberschreibung aller passenden Eintraege
-}

--c)	
aufgabe8c = ersetze (SI "Robert" 9) liste3
{-
Nichtleer (SI "Robert" 9)(Nichtleer (SI "Robert" 9) Leer)
- siehe b)
-}	

--d)	
aufgabe8d = findeAnIdx 1 liste3
{-
Doch (SI "Robert" 7)
- Rueckgabe des n-ten Eintrags der Liste als "Doch el"
- wenn es desen nicht gibt dann "Nicht"
-}

-- Aufgabe 9 arithmetische Ausdruecke
data Ausdruck = Konstante Integer
  | Variable String
  | Summe Ausdruck Ausdruck
  | Differenz Ausdruck Ausdruck
  | Produkt Ausdruck Ausdruck
  deriving (Show)

-- 9.1

-- a)
-- 0 - x
aufgabe9a = Differenz (Konstante 0) (Variable "x")

-- b)
-- 3 * x + y
aufgabe9b = Summe (Produkt (Konstante 3) (Variable "x")) (Variable "y")

-- c)
-- 3 * (x + y)
aufgabe9c = Produkt (Konstante 3) (Summe (Variable "x") (Variable "y"))

-- d)
-- 3 + x * y
aufgabe9d = Summe (Konstante 3) (Produkt (Variable "x") (Variable "y"))

-- 9.2

--3.2
{-
a)	
Produkt (Differenz (Variable "x") (Konstante 0)) (Variable "y")

(x - ) * y

b)	
Differenz (Variable "x") (Produkt (Konstante 0) (Variable "y"))

x - (0 * y)


c)	
Summe (Produkt (Konstante 3) (Variable "z")) (Konstante 5)

(3 * z) + 5
-}

-- 9.3

ausdruckNachString :: Ausdruck -> String
ausdruckNachString (Konstante zahl)     = show zahl
ausdruckNachString (Variable name)      = name
ausdruckNachString (Summe arg1 arg2)	  = "(" ++ ausdruckNachString arg1 ++ " + " ++ ausdruckNachString arg2 ++ ")"
ausdruckNachString (Differenz arg1 arg2)= "(" ++ ausdruckNachString arg1 ++ " - " ++ ausdruckNachString arg2	++ ")" 
ausdruckNachString (Produkt arg1 arg2) 	= ausdruckNachString arg1 ++ " * " ++ ausdruckNachString arg2

-- 10 Rekursion uber Listen

testlist :: [Integer]
testlist = [0,1,2,3,4,5,6,7,8,9]

-- 10.1
elementAnIdx :: Integer -> [el] -> el
elementAnIdx _ [] 		= error "liste leer oder position nicht drin"
elementAnIdx 0 (x:xs) = x
elementAnIdx i (x:xs) = elementAnIdx (i - 1) xs

-- 10.2
snoc :: [el] -> el -> [el] 
snoc [] o 	  = o : []
snoc (x:xs) o = x : snoc xs o

-- 10.3
rueckwaerts :: [el] -> [el]
rueckwaerts [] 		 = []
rueckwaerts (x:xs) = snoc (rueckwaerts xs) x

-- 10.4
praefix :: Integer -> [el] -> [el]
praefix _ []		= []
praefix 0 _ 		= []
praefix i (x:xs)= x : praefix (i - 1) xs 

-- 10.5
suffix :: Integer -> [el] -> [el]
suffix i l  = rueckwaerts $ praefix i (rueckwaerts l)
