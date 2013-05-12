qsort [] = []
qsort (x:xs) = qsort smalls ++ [x] ++ qsort larges where
      smalls = filter (<=x) xs
      larges = filter (>x) xs


rev [] = []
rev (x:xs) = rev xs ++ [x]


zipTogether :: [a] -> [b] -> [(a, b)]
zipTogether [] _ = []
zipTogether _ [] = []
zipTogether (x:xs) (y:ys) = (x, y) : zipTogether xs ys 

cipher :: [Char] -> Int -> [Char]
cipher [] _ = []
cipher str n = map (rotate n) str where
       rotate n chr | n == 0 || chr < 'a' || chr > 'z' = chr
                    | chr == 'z' = rotate (n-1) 'a'
                    | otherwise = rotate (n-1) $ succ chr


cipher2 s n = map (rotate2 n) s where
        rotate2 n c = ([c..'z'] ++ ['a'..c]) !! mod n 26
                    

isPal [] = True
isPal [_] = True
isPal (x:xs) = (x == last xs) && (isPal $ init xs)

isPal2 lst = reverse lst == lst


fac1 0 = 1
fac1 n = n * fac1 (n - 1)

fac2 n = fac' n 1 where
     fac' 0 acc = acc
     fac' n acc = fac' (n - 1) (n * acc)


