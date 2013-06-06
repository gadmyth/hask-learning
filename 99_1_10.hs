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
