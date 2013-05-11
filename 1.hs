module Test where

import HaskLearn

reportMeasurement :: Measurement -> String
reportMeasurement (HaskLearn.MetricMeasurement x u)
				  = (show x) ++ " " ++ (show u)

reportMeasurement m = reportMeasurement (convert m)

main = do
	 putStrLn "Hello, world!"
	 putStrLn "main is a function"
	 putStrLn "of type IO ()"


countChars :: String -> String
countChars str = unlines lengths where
		   lengths = map (show.length) allLines
		   allLines = lines


