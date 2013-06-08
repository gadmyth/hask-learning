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

-- | P7 Flatten a nested list structure. 

data NestedList a = Elem a | List [NestedList a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- | P8 liminate consecutive duplicates of list elements.
compress [] = []
compress lst = com' lst [] where
         com' [] cum = cum
         com' (x:xs) cum | null cum             = com' xs  (cum ++ [x]) 
                         | (last cum) == x      = com' xs cum
                         | otherwise            = com' xs (cum ++ [x])


-- | P9 Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack [] = []
pack lst = p' lst [] where 
     p' [] cum = cum
     p' (x:xs) cum | null cum = p' xs (cum ++ [[x]])
                   | (last $ last cum) == x = p' xs ((init cum) ++ [(last cum) ++ [x]])
                   | otherwise = p' xs (cum ++ [[x]])

-- | P10 Run-length encoding of a list.
--Use the result of problem P09 to implement the so-called run-length encoding data compression method.
--Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode [] = []
encode lst = e' lst [] where 
     e' [] cum = cum
     e' (x:xs) cum | null cum = e' xs (cum ++ [(1, x)])
                   | (snd $ last cum) == x = e' xs ((init cum) ++ [((fst $ last cum) + 1, x)])
                   | otherwise = e' xs (cum ++ [(1, x)])


