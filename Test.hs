module Test where

import HaskLearn

reportMeasurement :: Measurement -> String
reportMeasurement (HaskLearn.MetricMeasurement x u)
				  = (show x) ++ " " ++ (show u)

reportMeasurement m = reportMeasurement (convert m)

main = putStrLn "Hello, world!" >> putStrLn "main is a function" >> putStrLn "of type IO ()"

countChars :: String -> String
countChars str = unlines lengths where
		   lengths = map (show.length) allLines
		   allLines = lines str



piGuess :: Int -> Double
piGuess n = sum $ map f [1..n]
f :: Int -> Double
f x = let k = fromIntegral x in
  4*(-1)^(x+1) / (2.0*k-1) 
