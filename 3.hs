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


fac1 0 = 1
fac1 n = n * fac1 (n - 1)

fac2 n = fac' n 1 where
     fac' 0 acc = acc
     fac' n acc = fac' (n - 1) (n * acc)