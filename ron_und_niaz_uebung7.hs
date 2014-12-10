{-
ALP1 - Uebung 7
Ron Mizrahi und Niaz Faridani-Rad
Tutorium: Uebung 08 am Dienstag um 12
2014.12.08
-}

import Data.List

-- Aufgabe 1
delMax :: [Int] -> [Int]
delMax [] = []
delMax xs = delete (maximum xs) xs

selStep :: ([Int],[Int]) -> ([Int],[Int])
selStep ([],[]) = ([],[])
selStep (xs,ys) = (delMax xs , maximum xs : ys)

selSort :: [Int] -> [Int]
selSort [] = []
selSort xs = sshelper (xs,[])
	where
		sshelper :: ([Int],[Int]) -> [Int]
		sshelper ([],ys) =	ys
		sshelper (xs,ys) =	sshelper (selStep (xs,ys))
		
-- Aufgaben 2 und 3 wurden handschriftlich bearbeitet