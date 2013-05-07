import Control.Monad.Trans.Cont

ex1 = do
    a <- return 1
    b <- return 10
    return $ a + b

test1 = runCont ex1 show

ex2 = do
    a <- return 1
    b <- cont (\fred -> fred 10)
    return $ a + b

test2 = runCont ex2 show

ex3 = do
    a <- return 1
    b <- cont (\fred -> "escape")
    return $ a + b

test3 = runCont ex3 show

ex4 = do
    a <- return 1
    b <- cont (\fred -> fred 10 ++ fred 20)
    return $ a + b

test4 = runCont ex4 show

ex5 = do
    a <- return 1
    b <- cont (\fred -> concat [fred 10, fred 20])
    return $ a + b

test5 = runCont ex5 show

ex6 = do
    a <- return 1
    b <- cont (\fred -> [10, 20] >>= fred)
    return $ a + b

test6 = runCont ex6 return

i x = cont (\fred -> x >>= fred)
run m = runCont m return
test7 = run $ do
      a <- i [1, 2]
      b <- i [10, 20]
      return $ a + b
      
