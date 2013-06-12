-- | https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing
import Test.QuickCheck
import Data.List
import Data.Char as Char

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) ==
      reverse ys ++ reverse xs

split :: Char -> String -> [String]
split c [] = []
slpit c xs = xs' : if null xs'' then [] else split c (tail xs'')
      where xs' = takeWhile (/=c) xs
      	    xs'' = dropWhile (/=c) xs

join :: Char -> [String] -> String
join c = concat . intersperse [c]

--prop_join_split :: Char -> String -> Bool
--prop_join_split c xs = join c (split c xs) == xs

prop_join_split' xs = forAll (elements xs) $ \c -> 
		 join c (split c xs) == xs

instance Arbitrary Char where
	 arbitrary = choose ('\32', '\128')
	 coarbitrary c = variant (ord c `rem` 4)

main = quickCheck prop_join_split'
