{-
ALP1 - Uebung 3
Ron Mizrahi und Niaz Faridani-Rad
Tutorium: Uebung 08 am Dienstag um 12
2014.11.10
-}

-- Aufgabe 1
-- 1a) True wenn einer der Werte die Summe der anderen beiden Werte ist, sonst False
sumIncluded3 :: Int -> Int -> Int -> Bool	
sumIncluded3 a b c
	|	a + b == c		=	True
	|	a + c == b		=	True
	|	b + c == a		=	True
	|	otherwise		=	False

-- 1b) True wenn einer der Werte die Summe von zwei anderen Werten ist, sonst False
sumIncluded4 :: Int -> Int -> Int -> Int -> Bool			
sumIncluded4 a b c d
	|	a + b == c		=	True
	|	a + b == d		=	True
	|	a + c == b		=	True
	|	a + c == d		=	True
	|	a + d == b		=	True
	|	a + d == c		=	True
	|	b + c == a		=	True
	|	b + c == d		=	True
	|	b + d == c		=	True
	|	b + d == a		=	True
	|	c + d == a		=	True
	|	otherwise		=	False	

-- 1c) Differenz aus dem groessten und kleinsten Wert
spanOf3 :: Float -> Float -> Float -> Float					
spanOf3 a b c = maximum [a,b,c] - minimum [a,b,c]
	
-- 1d) Erhält zwei Int-Werte, müssen 1, 2 oder 3 sein und verschieden sein, fehlender Wert wird bestimmt
thirdMan :: Int -> Int -> Int								
thirdMan a b
	|	a == 2 && b == 3	=	1
	|	a == 3 && b == 2	=	1
	|	a == 1 && b == 3	=	2
	|	a == 3 && b == 1	=	2
	|	a == 1 && b == 2	=	3
	|	a == 2 && b == 1	=	3
	|	otherwise			=	error "Bitte geben Sie die Werte aus der Menge {1,2,3} ein, wobei die beiden Werte verschieden sein muessen."
	
	
-- Aufgabe 2
-- Da die Divisionsfunktion nicht funktionieren wollte und es Probleme mit negativen Variablen gab, schrieben wir eine funktionierende Hilfe 
divisionshilfe :: Float -> Float -> Float
divisionshilfe xg yg
	| (xg /= 0)		=	(-yg / xg)
	| otherwise 	=	error "xg = 0 und durch 0 teilen ist nicht möglich"

-- 2a) Erhaelt zwei Werte xg und yg, welche die Gerade g mit Hilfe des Punktes (xg,yg) darstellen, sowie einen Wert x, wobei y so berechnet wird, das der Punkt (x,y) auf der Geraden g liegt.
valueAt :: Float -> Float -> Float -> Float								
valueAt xg yg x = (divisionshilfe yg xg) * x + yg

-- 2b) Erhaelt jeweils 2 Punkte von 2 Geraden und überprüft mit Hilfe der Steigung, ob sie parallel sind, da die Steigung zweier Geraden gleich sein muss, damit jene parallel sind.
testParallel :: Float -> Float -> Float -> Float -> Bool				
testParallel xg yg xh yh 
	| (divisionshilfe yg xg) == (divisionshilfe yh xh) 		=	True 
	| otherwise 											=	False
	
-- 2c) Zwei Geraden sind parallel, wenn ihre Steigungen gleich sind
parallelThroughX :: Float -> Float -> Float -> Float					
parallelThroughX xg yg x = yg/xg*x 

-- 2d) Fuer Schnittpunktbestimmung: Gleichsetzung der Geraden g(x) = h(x), Umformung auf x
crossingAt :: Float -> Float -> Float -> Float -> Float
crossingAt xg yg xh yh
	| (divisionshilfe xh yh /= divisionshilfe xg yg) 	=	(yg - yh) / (divisionshilfe xh yh - divisionshilfe xg yg)
	|  otherwise 										=	error "Die Geraden sind parallel zueinander und schneiden sich deshalb nicht"

-- 2e1)	ax + by = c ist aequivalent zu y = (c-ax)/b und x = (c-by)/a, sodass sich durch (xg/0) und (0/yg) ergibt: y = c/b und x = c/a. 
--		Wenn c = 0, dann x,y = 0, also Punkt (0/0). Wenn b = 0, dann Gerade parallel zur Y-Achse. Wenn a = 0, dann Gerade parallel zur X-Achse.
computeXg :: Float -> Float -> Float -> Float															
computeXg a b c 																						
	| a == 0 || b == 0 || c == 0 				=	error "Input out of scope" 
	| otherwise 								=	c / a

-- 2e2)
computeYg :: Float -> Float -> Float -> Float															
computeYg a b c
	| a == 0 || b == 0 || c == 0 				=	error "Input out of scope"
	| otherwise									=	c / b


-- Aufgabe 3
-- Primzahltest mit besserer Laufzeit, als Skript-Code
primtest :: Int -> Bool
primtest n
	| n <= 1 								=	False
	| n == 2 								=	True
	| otherwise 							=	help n 2 			-- Rekursionsbeginn
	where
		help :: Int -> Int -> Bool
		help n k
			| mod n k == 0 					=	False 				-- Fuer k wofür gilt, n % k = 0, ist n keine Primzahl
			| mod n k /= 0 && k*k >= n 		=	True 				-- Rekursionsende
			| mod n k /= 0 					=	help n (k+1) 		-- Inkrementelle Rekursion





	
	
	