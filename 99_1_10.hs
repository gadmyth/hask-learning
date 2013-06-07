-- | http://www.haskell.org/haskellwiki/99_questions/1_to_10

-- | P1 Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Empty list has no last!"
myLast (x:xs) | null xs = x
              | otherwise = myLast xs


-- | P2 Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty List!"
myButLast [_] = error "List has only one element!"
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs

-- | P3 Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Num b, Eq b) => [a] -> b -> a
elementAt [] _ = error "List is empty!"
elementAt (x:xs) n
          | n == 1 = x
          | otherwise = elementAt xs (n - 1)

-- | P4 Find the number of elements of a list.  
myLength :: Num b => [a] -> b
myLength lst = ml' lst 0 where
         ml' [] cum = cum
         ml' (_:xs) cum = ml' xs (cum + 1)

-- | P5 Reverse a list.
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- | P6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPal [] = True
isPal [_] = True
isPal (x:xs) = (x == last xs) && isPal (init xs)
