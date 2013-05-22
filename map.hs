-- http://www.haskell.org/ghc/docs/6.12.2/html/libraries/containers-0.3.0.0/Data-Map.html

import Prelude hiding (lookup)
import Data.Map

employeeDept = fromList [("John", "Sales"), ("Bob", "IT")]
deptCountry = fromList [("IT", "USA"), ("Sales", "France")]
countryCurreny = fromList [("USA", "Dollar"), ("France", "Euro")]

employeeCurrency :: String -> Maybe String
employeeCurrency name = do
                 dept <- lookup name employeeDept
                 country <- lookup dept deptCountry
                 lookup country countryCurreny

employeeCurrency2 employee = lookup employee employeeDept >>=
                  (\dept -> lookup dept deptCountry) >>=
                  (\country -> lookup country countryCurreny)


main = (putStrLn $ "John's currency: " ++ (show (employeeCurrency2 "John"))) >>
     (putStrLn $ "Pete's currency: " ++ (show (employeeCurrency2 "Pete")))
