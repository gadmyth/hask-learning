-- | P11 Modified run-length encoding.
--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
--Only elements with duplicates are transferred as (N E) lists.

data Count a b = Single a | Multiple b a deriving (Show, Eq)

inc (Single x) = Multiple 2 x
inc (Multiple n x) = Multiple (n+1) x
val (Single x) = x
val (Multiple _ x) = x

mEcode lst = me' lst [] where
       me' [] cum = cum
       me' (x:xs) [] = me' xs [Single x]
       me' (x:xs) cum | val (last cum) == x = me' xs ((init cum) ++ [inc $ last cum])
                      | otherwise = me' xs (cum ++ [Single x])

-- | P12 Decode a run-length encoded list.
dec (Multiple 2 x) = Single x
dec (Multiple n x) = Multiple (n-1) x
dec (Single x) = error "Count should be not zero!"
mDecode mark = md' mark [] where
        md' [] lst = lst
        md' ((Single x):xs) lst = md' xs (lst ++ [x])
        md' ((count@(Multiple _ x)):xs) lst = md' ((dec count):xs) (lst ++ [x])

-- | P14 Duplicate the elements of a list.
dupli lst = dup' lst [] where
      dup' [] cum = reverse cum
      dup' (x:xs) cum = dup' xs (x:x:cum)

-- | P15 Replicate the elements of a list a given number of times.
repli lst n = dup' lst [] n where
      dup' [] cum _ = reverse cum
      dup' (x:xs) cum n = dup' xs ((replicate n x) ++ cum) n
