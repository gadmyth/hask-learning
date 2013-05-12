-- lfp
y f = f (y f)
fac7 = y (\f n -> if (n == 0) then 1 else n * f (n-1))