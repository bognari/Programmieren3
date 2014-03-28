module AVL where

-- Aufgabe 13 (AVL-Bäume)

data AVLBaum el = AVLBlatt | AVLKnoten Int (AVLBaum el) el (AVLBaum el)

avlb = AVLKnoten 1 (AVLBlatt) 'A' (AVLBlatt)

hoehe :: AVLBaum el -> Int
hoehe AVLBlatt            = 0
hoehe (AVLKnoten h _ _ _) = h

balance :: AVLBaum el -> AVLBaum el -> Int
balance left right = (hoehe left) - (hoehe right)

avlKnoten :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
avlKnoten left root right = AVLKnoten h left root right
  where
    h = 1 + max (hoehe left) (hoehe right)
    
-- 13.1
verbinden :: AVLBaum el -> el -> AVLBaum el -> AVLBaum el
verbinden t1@(AVLKnoten h1 l1 w1 r1) r t2@(AVLKnoten h2 l2 w2 r2) 
  | balance t1 t2 == 2  && (balance l1 r1) < 0  = leftRightRotation (avlKnoten t1 r t2)
  | balance t1 t2 == -2 && (balance l2 r2) > 0  = rightLeftRotation (avlKnoten t1 r t2)
  | balance t1 t2 == 2  && (balance l1 r1) >= 0  = leftRotation (avlKnoten t1 r t2)
  | balance t1 t2 == -2 && (balance l2 r2) <= 0  = rightRotation (avlKnoten t1 r t2)
  | otherwise = avlKnoten t1 r t2
                
leftRightRotation :: AVLBaum el -> AVLBaum el
leftRightRotation = rightRotation.leftRotation

rightLeftRotation :: AVLBaum el -> AVLBaum el
rightLeftRotation = leftRotation.rightRotation

leftRotation :: AVLBaum el -> AVLBaum el
leftRotation (AVLKnoten h (AVLKnoten h1 ll lv lr) v r)
  = AVLKnoten h' ll lv (AVLKnoten h'' lr v r) where
  h'  = 1 + max (hoehe ll) h''
  h'' = 1 + max (hoehe lr) (hoehe r)

rightRotation :: AVLBaum el -> AVLBaum el
rightRotation (AVLKnoten h l v (AVLKnoten hr rl rv rr))
  = AVLKnoten h' (AVLKnoten h'' l v rl) rv rr where
  h'  = 1 + max h'' (hoehe rr)
  h'' = 1 + max (hoehe l) (hoehe rl)

-- 13.2

avlEinfuegen :: (Ord el) => el -> AVLBaum el -> AVLBaum el
avlEinfuegen a AVLBlatt = AVLKnoten 1 AVLBlatt a AVLBlatt
avlEinfuegen a t@(AVLKnoten h l w r)
	| a > w     = verbinden l w (avlEinfuegen a r)
	| a < w     = verbinden (avlEinfuegen a l) w r
	| otherwise = t

-- 13.3

avlLoeschen :: (Ord el) => el -> AVLBaum el -> AVLBaum el
avlLoeschen _ AVLBlatt = AVLBlatt
avlLoeschen a (AVLKnoten h l w r)
	| a < w  = verbinden (avlLoeschen a l) w r
	| a > w  = verbinden l w (avlLoeschen a r)
	| a == w = neubauen l r
		where
			neubauen l AVLBlatt = l
			neubauen AVLBlatt r = r
			neubauen l r        = verbinden l' w' r
			l' = avlLoeschen w' l
			w' = groesster l
			groesster (AVLKnoten _ AVLBlatt z AVLBlatt) = z
			groesster (AVLKnoten _ l _ r)               = groesster r
