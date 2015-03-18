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

--orderThree
orderThree xs = [2,3,4]