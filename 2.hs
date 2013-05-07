import qualified Data.Map as Map

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook = [("betty" , "444-2938")
		  	,("bonnie", "452-2928")
			,("patsy", "493-2928")
			,("lucille", "205-2928")
			,("wendy", "989-9292")
			,("penny", "853-2492")
			]

isPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
isPhoneBook name number pbook = (name, number) `elem` pbook

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map = case Map.lookup number map of
			 Nothing -> Left $ "Locker number " ++ show number ++ " doesn't exist"
			 Just (state, code) -> if state /= Taken then Right code else Left $ "Locker " ++ show number ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD39I"))
		  			   ,(101, (Free, "JAH3I"))
					   ,(103, (Free, "IQSA9"))
					   ,(105, (Free, "QOTSA"))
					   ,(109, (Taken, "893JJ"))
					   ,(110, (Taken, "99292"))]