qsort [] = []
qsort (x:xs) = qsort smalls ++ [x] ++ qsort larges where
      smalls = filter (<=x) xs
      larges = filter (>x) xs


rev [] = []
rev (x:xs) = rev xs ++ [x]

