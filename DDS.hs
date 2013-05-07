data Tree = Leaf | Node Tree Tree
-- | the Left and Right means the branch or path you choose,
-- | and Tree means rest of the tree not on our path
data Tree'  = Left Tree | Right Tree
type Zipper = [Tree']

-- | fill a hole with a tree, the snd parameter is a plugin, the real PARENT tree info is in the Zipper
plug :: Zipper -> Tree -> Tree
plug [] tree = tree
plug ((Left right) : z) tree = Node (plug z tree) right
plug ((Right left) : z) tree = Node left (plug z tree)