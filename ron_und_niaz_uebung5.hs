{-
ALP1 - Uebung 5
Ron Mizrahi und Niaz Faridani-Rad
Tutorium: Uebung 08 am Dienstag um 12
2014.11.24
-}

import Data.Char
import Data.List

-- Aufgabe 1
-- Aufgabe 1a
superFib :: Int -> Int
superFib x
	| x == 0	= 0 
        | x == 1	= 0
        | x == 2	= 1                                                                  
        | otherwise	= (superFib (x-1)) + (superFib (x-2)) + (superFib (x-3))
       
-- Aufgabe 1b
countCalls :: Int -> Int
countCalls a
	| a==0		=1
	| a==1		=1
	| a==2		=1
	| otherwise	= 1 + (countCalls (a-1)) + (countCalls (a-2)) + (countCalls (a-3))
	
countCallsList :: [Int]
countCallsList = [countCalls 1, countCalls 2, countCalls 3, countCalls 4, countCalls 5, countCalls 6, countCalls 7, countCalls 8, countCalls 9, countCalls 10, countCalls 11, countCalls 12, countCalls 13, countCalls 14, countCalls 15, countCalls 16, countCalls 17, countCalls 18, countCalls 19, countCalls 20]

-- Aufgabe 2
type Zeit = (Int,Int)

-- Aufgabe 2a
addStd :: Zeit -> Int -> Zeit 
addStd (stunde,minute) z
	| stunde+z <= 23 && minute <= 59 	= ((stunde+z),minute)
	| stunde+z <= 23 && minute > 59 	= (((stunde+z+(minute `div` 60))) `mod` 24,minute `mod` 60)
	| stunde+z >  23 && minute <= 59 	= ((stunde+z) `mod` 24,minute)
	| stunde+z >  23 && minute > 59 	= ((stunde+z+(minute `div` 60)) `mod` 24,minute `mod` 60)

addMin :: Zeit -> Int -> Zeit    
addMin (stunde,minute) z
        | stunde <= 23 && minute+z <= 59 	= (stunde,minute+z)
        | stunde > 23 && minute+z <= 59 	= (stunde `mod` 24,minute+z)
        | stunde <= 23 && minute+z > 59 	= (stunde + ((minute+z) `div` 60) `mod` 24,(minute+z) `mod` 60)
        | stunde > 23 && minute+z > 59 		= (stunde + ((minute+z) `div` 60) `mod` 24,(minute+z) `mod` 60)
        
-- Aufgabe 2b
dauer :: Zeit -> Zeit -> Zeit
dauer (stunde,minute) (stunde2,minute2)
	| stunde>stunde2 && minute>minute2	= (((stunde-stunde2) + ((minute-minute2) `div` 60)),(minute-minute2) `mod` 60)
	| stunde==stunde2 && minute>minute2	= (0,(minute-minute2))
	| stunde<stunde2 && minute>minute2	= (((stunde2-stunde) + ((minute2-minute) `div` 60)),(minute2-minute) `mod` 60)	   
	| stunde>stunde2 && minute==minute2	= ((stunde-stunde2),0)
	| stunde==stunde2 && minute==minute2	= (((stunde-stunde2) + ((minute-minute2) `div` 60)),(minute-minute2) `mod` 60)
	| stunde<stunde2 && minute==minute2	= ((stunde2-stunde),0)
	| stunde>stunde2 && minute<minute2	= (((stunde-stunde2) + ((minute-minute2) `div` 60)),(minute-minute2) `mod` 60)	
	| stunde==stunde2 && minute<minute2	= (0,minute2-minute)
   	| stunde<stunde2 && minute<minute2	= (((stunde2-stunde) + ((minute2-minute) `div` 60)),(minute2-minute) `mod` 60)
          
-- Aufgabe 2c
anzeige :: Zeit -> [Char]
anzeige (x,y) 
        | y > 9		= (show x) ++ ":" ++ (show y) ++ " Uhr"
        | y <= 9	= (show x) ++ ":0" ++ (show y) ++ " Uhr"
      
  
-- Aufgabe 3
deleteABs :: [Char] -> [Char]
deleteABs xs 
	| elem 'A' xs == True		= deleteABs (delete 'A' xs)
	| elem 'B' xs == True		= deleteABs (delete 'B' xs)
        | otherwise 			= xs

doubleDigits :: [Char] -> [Char]
doubleDigits [] = []
doubleDigits (x:xs)
	| elem x ['1'..'9']		= x:x:doubleDigits xs
	| elem x ['A'..'Z']		= x:doubleDigits xs
	| otherwise			= error "Bitte Eingabe anpassen."

shiftDigits :: [Char] -> [Char]
shiftDigits [] = []
shiftDigits (x:xs)		
	| elem x ['1'..'9']		= (chr(ord x+1)):shiftDigits xs			
	| elem x ['A'..'Z']		= x:shiftDigits xs
	| otherwise			= error "Bitte Eingabe anpassen."


-- Aufgabe 4
-- Aufgabe 4a
countPos :: [Int] -> Int
countPos xs
	| xs == []		= 0
	| otherwise		= length(filter (>=0) xs)

-- Aufgabe 4b
monIncrPrefix :: [Int] -> [Int]
monIncrPrefix [] = []
monIncrPrefix (x:xs) = miphelper x xs
	where
	miphelper :: Int -> [Int] -> [Int]
	miphelper n (x:xs)
		| n > x		 = [n]
		| otherwise	 = n: (miphelper x xs)

-- Aufgabe 4c
parSum :: [Int] -> [Int]
parSum ys = scanl1 (+) ys
