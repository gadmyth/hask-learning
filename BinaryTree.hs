module BinaryTree (Tree(..), makeTree, add, printTree) where

data Tree = Tree Int Tree Tree | EmptyTree deriving (Show, Eq)

printTree :: Tree -> IO()
printTree tree = putStrLn (printSubTree tree 0 0)

nodeLinks = ["", "/", "\\"]

printSubTree :: Tree -> Int -> Int -> String
printSubTree EmptyTree _ _ = ""
printSubTree (Tree root left right) n link = (printSubTree left (n+3) 1)
              ++ (replicate n ' ')
              ++ (nodeLinks !! link)
              ++ (show root) ++ "\n"
              ++ (printSubTree right (n+3) 2)


makeTree :: Int -> Tree
makeTree n
         | n < 0 = EmptyTree
         | otherwise = Tree n (makeTree (n - 1)) (makeTree (n - 2))


add :: Tree -> Int
add EmptyTree = 0
add (Tree root left right) = root + add left + add right