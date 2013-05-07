x -: f = f x

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

goLeft ((Node v l r), crumbs) = (l, ((LeftCrumb v r):crumbs))
goLeft (Empty, crumbs) = (Empty, crumbs)

goRight ((Node v l r), crumbs) = (r, ((RightCrumb v l):crumbs))
goRight (Empty, crumbs) = (Empty, crumbs)

goUp (tree, []) = (tree, [])
goUp (l, ((LeftCrumb v r):crumbs)) = ((Node v l r), crumbs)
goUp (r, ((RightCrumb v l):crumbs)) = ((Node v l r), crumbs)
