module ChapterThree where

inList :: Eq a => a -> [a] -> [a]
inList targ xs = [x | x <- xs , targ == x ]
--inList targ xs = targ == xs

square :: (Num a) => a -> a
square x = x*x

squareEvenNumbers :: [Int] -> [Int]
squareEvenNumbers xs = [ square x | x <- xs , even x ]

courseMajor :: String -> String
courseMajor all@(x:y:ys) = all ++ " is a " ++ x:[y] ++ " course"

threshold :: (Ord a, Num a) => a -> a -> a -> String 
threshold x y z 
	| total < low = "Total is low"
	| total < medium = "Total is medium"
	| total < high = "Total is high"
	| total >= high = "Total is extraordinary"
	where
		total = x*y
		low = z
		medium = 2*z
		high = 3*z

lactate :: (Ord a, Fractional a) => a -> a -> String
lactate hr maxhr 
	| level < 0.7 = "warmup"
	| level < 0.8 = "aerobic"
	| level < 0.9 = "steadyState"
	| level < 1.0 = "anaerobic"
	| otherwise = "wow, don't do this for long!"
	where
		level = hr / maxhr

calcAreas :: (Num t) => [(t, t)] -> [t]
calcAreas xs = [ area x y | (x, y) <- xs]
	where area x y = x*y

calcTriangleAreas :: (Fractional t) => [(t, t)] -> [t]
calcTriangleAreas xs = [ triangleArea x y | (x, y) <- xs]
	where triangleArea x y = x*y/2

orderTwo :: (Ord a) => [a] -> [a]
orderTwo (x:y:ys) 
	| x < y = x:[y]
	| otherwise = y:[x]

orderThree :: (Ord a) => [a] -> [a]
orderThree (x:y:z:zs)
	| x == y = x:y:[z]
	| head( orderTwo (x:[y]) ) == head( orderTwo (x:[z]) ) = x:(orderTwo (y:[z]) )
	| head( orderTwo (x:[y]) ) == head( orderTwo (y:[z]) ) = y:(orderTwo (x:[z]) )
	| head( orderTwo (x:[z]) ) == head( orderTwo (y:[z]) ) = z:(orderTwo (y:[x]) )   

{-
Guards vs Pattern Matching

Guards- Use when you need to make a choice that doesn't neatly correspond to a pattern
	  - Usually more readable if you need to access whole structure
	  - Guards are great for catching exceptions in the invalid input case and with other things.
	  - like an if/else
	  - typically start thinking about if a pattern starts getting convoluted or ugly

Patterns- Use for things that can be matched one to two elements deep
	    - Checking for empty lists
	    - Great for deconstructing data
	    - like a case statement
	    - use for when you don't care about the data as a whole
	    - use when something can be trivially checked
	    - patterns are more of a default use
-}