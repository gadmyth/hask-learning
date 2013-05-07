module HaskLearn (MetricUnit(..), ImperialUnit(..), Measurement(..), convert) where

pow a n | n <= 0 = 1
 	| otherwise = a * (pow a (n - 1))


--isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal [_] = True
isPal (x:xs) = if x == last xs then isPal $ init xs
	  else False


head1 :: [a] -> a
head1 [] = error "empty list"
head1 [a] = a
head1 (a:as) = a

head' :: [a] -> a
head' xs | null xs = error "empty list"
	  	 | otherwise = xs !! 0


convert' :: (Double, [Char]) -> (Double, [Char])
convert' (num, unit)
		| unit == "m" = (num * 1.09361, "yd")
		| unit == "yd" = (num * 0.9144027, "m")
		| unit == "L" = (num * 0.264172, "gal")
		| unit == "gal" = (num * 3.7854128, "L")
		| unit == "kg" = (num * 2.20462, "lb")
		| unit == "lb" = (num * 0.45359293, "kg")
		| otherwise = error "Invalid input"


data MetricUnit = Meter | Liter | Kilogram deriving (Show, Eq)
symbol :: MetricUnit -> [Char]
symbol u | u == Meter = "m"
	     | u == Liter = "L"
		 | u == Kilogram = "kg"


data ImperialUnit = Yard | Gallon | Pound deriving (Show, Eq)
data Measurement = MetricMeasurement Double MetricUnit | ImperialMeasurement Double ImperialUnit deriving (Show, Eq)
convert :: Measurement -> Measurement
convert (MetricMeasurement x u)
		| u == Meter = ImperialMeasurement (1.0936 * x) Yard
		| u == Liter = ImperialMeasurement (0.2642 * x) Gallon
		| u == Kilogram = ImperialMeasurement (2.2046 * x) Pound

convert (ImperialMeasurement x u)
		| u == Yard = MetricMeasurement (0.9144 * x) Meter
		| u == Gallon = MetricMeasurement (3.7854 * x) Liter
		| u == Pound = MetricMeasurement (0.4536 * x) Kilogram

	

data Point = Point {x::Double, y::Double}


data Tree = Nil | Tree {val :: Int, left :: Tree, right :: Tree} deriving (Show)
add :: Tree -> Int
add Nil = 0
add tree = val tree + add (left tree) + add (right tree)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs