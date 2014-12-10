{-
ALP1 - Uebung 6
Ron Mizrahi und Niaz Faridani-Rad
Tutorium: Uebung 08 am Dienstag um 12
2014.12.01
-}

import Data.Char
import Data.List
import Data.String

{- 
-- Aufgabe 1
-- Aufgabe 1a				-- Beweis mit struktureller Induktion, dass für beliebige Int-Listen xs die (Wert-) Identität ping xs = - pong xs gilt
ping xs = - pong xs

IA: ping []   	= 0	  				| (1)
	- pong [] 	= -0 = 0 			| (3)
IV: ping xs 	= - pong xs 		| für eine beliebige int liste 
IB: ping (x:xs)	= - pong (x:xs)		| für eine beliebige aber feste int liste (x:xs) gilt
IS: ping (x:xs) = - pong (x:xs) 	| (2)
	x + pong xs = -(-x + ping xs)	| (doppelte negation)
	x + pong xs = x - ping xs		| (IV)
	x + pong xs = x - -pong xs  	| (doppelte negation)
	x + pong xs = x + pong xs	
									[q.e.d!]

-- Aufgabe 1b				-- Beweis mit struktureller Induktion, dass für beliebige Int-Listen ys und jeden Int-Wert x die Wert-Identitmagic 1 ys = 2^{length ys} * product ys gilt
magic x ys = x*2^length ys * product ys

IA: magic x [] = x						   					| (5)
	x*2^length [] * product [] = x*1*1 = x 					| (1) && (3)
IV: magic x ys = x*2^length ys * product ys 				| für eine beliebige Int-Liste und jeden Int-Wert
IB: magic x (y:ys) = x*2^length (y:ys) * product (y:ys)		| für eine beliebige aber feste Int-Liste und jeden Int-Wert
IS: magic x (y:ys) = 2^(length (y:ys)) * product (y:ys)  	| (6)
	magic (2*x*y) xs = 2^(length (y:ys)) * product (y:ys)	| (2)
	magic (2*x*y) xs = 2^(1 + length xs) * product (y:ys)	| (4)
	magic (2*x*y) xs = 2^(1 + length xs) * y * product ys	| (Potenzgesetz)
	magic (2*x*y) xs = 2*2^(length xs) * y * product ys  	| (IV)
	magic (2*x*y) xs = y*2* magic x ys                   	| (umformen)
	magic (2*x*y) xs = magic (2*x*y) ys					 	| (da ys eine beliebige liste sein kann kann sie also auch xs sein demnach gilt ys=xs)
	magic (2*x*y) xs = magic (2*x*y) xs
															[q.e.d!]
-}

-- Aufgabe 2
test = "abbacxxxax"

-- Aufgabe 2a
ignoreDoublings :: String -> String				-- alle Doppel- und Mehrfachzeichen durch ein einzelnes ersetzen
ignoreDoublings (x:y:xs)						
	| x == y 			= ignoreDoublings (y:xs)
	| otherwise 		= x:(ignoreDoublings (y:xs))
ignoreDoublings y = y

-- Aufgabe 2b
deleteRepetitions :: String -> String			-- jedes Zeichen, dass im Eingabestring vorkommt, nur einmal in den Ausgabestring setzen
deleteRepetitions = drHelper []
    where
    	drHelper gesehen [] = gesehen
        drHelper gesehen (x:xs)
        	| elem x gesehen 	= drHelper gesehen xs
            | otherwise 		= drHelper (gesehen ++ [x]) xs   
	-- ist äquivalent zu
deleteRepetitions2 :: String -> String
deleteRepetitions2 [] = []
deleteRepetitions2 (x:xs) = x : deleteRepetitions2 (myfilter (\y -> (x /= y)) xs)
	where
		myfilter :: (z -> Bool) -> [z] -> [z]			-- selbstgeschriebene Filter-Funktion
		myfilter bedingung [] = []
		myfilter bedingung (x:xs) = y ++ (myfilter bedingung xs)
			where y = if (bedingung x) then [x] else []
			
-- Aufgabe 2c
onlySingles :: String -> String		-- nur die Zeichen in den Ausgabestring setzen, die im Eingabestring genau einmal vorkommen
onlySingles xs = helper xs [] [] 	-- 2 Akkumulatoren, um eine Liste, mit nur den vorhandenen Single-Elementen, und eine, mit den restlichen Elementen zu haben
	where
		helper :: String -> String -> String -> String
		helper [] ys zs = zs 
		helper (x:xs) ys zs
			| (mynot(elem x ys)) &&( mynot(elem x xs)) = helper xs ys (zs++[x])
			| otherwise = helper xs (ys++[x]) zs
			where
				mynot :: Bool -> Bool	-- selbstgeschriebene Not-Funktion
				mynot True = False 
				mynot False = True
	
-- Aufgabe 2d			
countSymbols :: String -> [(Char, Int)]			-- bestimmt die Vielfachheit für alle in der Liste auftretenden Symbole
countSymbols xs = helper xs []
	where
		helper (x:[]) ys = count x ys
		helper (x:xs) ys = helper xs (count x ys)
		count x [] = [(x,1)]
		count x ((a,b):ys)
			| x == a		= [(a,b+1)] ++ ys
			| otherwise		= [(a,b)] ++ count x ys
		
-- Aufgabe 3
--3a
rev :: [a] -> [a]
rev [x] = [x]
rev (x:xs) = rev(xs) ++[x]

istPrim :: Int -> Int-> Bool 
istPrim n b
	|b == 0 		= True
	|b == 1 		= True
	|mod n b == 0 	= False
	|otherwise 		= istPrim n (b-1) 

pPz :: Int -> [Int] -> [Int]
pPz n (y:ys)
	|y < 2 			= []
	|n < 2 			= []
	|mod n y == 0 	= [y] ++ (pPz (div n y) (rev([x|x<-[2..y],istPrim x (div x 2)])))	
	|otherwise 		= pPz n ys 
	
len :: [a] -> Int
len xs = helper xs 0
	where
		helper :: [a] -> Int -> Int
		helper [] n 	= n
		helper (x:[]) n = 1+n 
		helper (x:xs) n = helper xs (n+1)

prodOf2Primes :: Int -> [Int]		-- berechnet bei Eingabe n die Liste aller Zahlen aus [4..n], deren Primproduktzerlegung aus genau zwei Faktoren besteht
prodOf2Primes n = [x|x<-[4..n], len(pPz x (rev([y|y<-[2..x], istPrim y (div y 2)]))) == 2]

-- Aufgabe 3b
squareNearlyInt :: Float -> [Float]		-- für einen positiven Float-Wert x mit höchstens einer Stelle hinter dem Komma, die Liste aller Float-Werte y erstellen, die auch höchstens eine Stelle hinter dem Komma haben, die Ungleichung 0 <= y <= x erfüllen und deren Quadrat höchstens 0.01 von einer ganzen Zahl entfernt ist
squareNearlyInt x = [y | y <- mapDiv10 ([0.0..(x*10)]), (mod (int (y*10)) 1 == 0) && pow2nextInt (y^2)]
	where
		mapDiv10 (x:[]) = [x/10]
		mapDiv10 (x:xs) = (x/10) : mapDiv10 (xs)
		int :: Float -> Int
		int x
			| x < 1 	= 0
			| otherwise = 1 + int (x-1)
		pow2nextInt y
			| y <= 1 && (y <= 0.01 || y >= 1 - 0.01)	= True
			| y <= 1 && (y >= 0.01 && y <= 1 - 0.01)	= False
			| otherwise 								= pow2nextInt (y-1)
			
-- Aufgabe 3c
mirrorCapitals :: String -> String		-- extrahiert alle Grossbuchstaben aus einem String (alle anderen Zeichen werden gestrichen) und spiegelt sie danach in der Mitte
mirrorCapitals xs = [(revASCII y) | y <- xs, fromEnum y >= 65 && fromEnum y <= 90]
	where
		revASCII y = toEnum ((my_abs ((fromEnum y) -90)) +65) 
			where
				my_abs y
					| y < 0 	= -y
					| otherwise 	= y
