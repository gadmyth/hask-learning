import Safe (readMay)

displayAge Nothing = putStrLn "You provided an invalid year"
displayAge (Just age) = putStrLn $ "In 2020, you will be: " ++ show age

main = do
     putStrLn "Please enter your birth year"
     yearStr <- getLine
     putStrLn "Please enter year in future"
     fyearStr <- getLine
     let age = readMay fyearStr >>= (\fyear -> (readMay yearStr >>= (\year -> Just (fyear - year))))
     displayAge age
     