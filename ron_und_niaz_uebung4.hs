{-
ALP1 - Uebung 4
Ron Mizrahi und Niaz Faridani-Rad
Tutorium: Uebung 08 am Dienstag um 12
2014.11.17
-}

import Data.Char (ord, chr)				--Fuer Aufgabe 2b

-- Aufgabe 1
-- 1a) Auswertung von Polynomen vom Grad 3
eval3 :: Float -> Float -> Float -> Float -> Float -> Float
eval3 r a3 a2 a1 a0
	| a3 == 0 || a2 == 0 || a1 == 0 || a0 == 0		=	error "Die Koeffizienten duerfen nicht 0 sein."
	| otherwise										=	(a3 * r^3) + (a2 * r^2) + (a1 * r) + a0
		
-- 1b) Testet die Existenz von reelen Nullstellen bei Polynomen vom Grad <= 2	
nullstellenTest2 :: Float -> Float -> Float -> Bool
nullstellenTest2 a2 a1 a0	
	| diskriminante >= 0		= True
	| diskriminante < 0			= False
  where 	p = a1/a2;
			q = a0/a2;
			diskriminante = ((a1^2) - (4*a2*a0));
			wurzel = sqrt diskriminante

-- 1c) Berechnet, bei Polynomen vom Grad <= 2, den kleinsten Absolutbetrag, den das Polynom bei der Auswertung annehmen kann
smallestVal :: Float -> Float -> Float -> Float
smallestVal a2 a1 a0
	| nullstellenTest2 a2 a1 a0 == True			=	sqrt(((a1^2) - (4*a2*a0)))
	| otherwise									=	(a0-((a1^2)/(4*a2)))
													
-- Aufgabe 2
-- 2a) Zahl m aus Z(k,n) zyklisch um s Einheiten verschieben. Wenn m nicht in Z(k,n) liegt, soll m ausgegeben werden.
cyclicShift :: Int -> Int -> Int -> Int -> Int
cyclicShift k n m s
	| m >= (k+n)								=	m
	| m < (k+n)									=	(mod ((m+s) - k) n) + k
	
-- 2b) Zeichen ch zyklisch um k Stellen verschieben, wenn ch ein Grossbuchstabe. Sonst ch unverändert.
caesarForCapitals :: Char -> Int -> Char
caesarForCapitals ch k            
	| 'A' <= ch && ch <= 'Z'		=	chr(((ord ch + k - 13) `mod` 26) + ord 'A')   
	| otherwise						=	ch 

-- 2c) Zeichen ch1 im zyklischen Alphabet um das Zentrum ch2 spiegeln. Einer der Eingaben kein Grossbuchstabe, ch1 ausgeben.
mirrorForCapitals :: Char -> Char -> Char
mirrorForCapitals ch1 ch2
	| ch1 >= 'A' && ch1 <= 'Z' && ch2 >= 'A' && ch2 <= 'Z'		=	chr ((mod ((ord ch1) + 2 * (ord ch2 - ord ch1) - ord 'A') 26) + ord 'A')
	| otherwise													=	ch1










	
	
	