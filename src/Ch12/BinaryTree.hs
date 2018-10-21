module Ch12.BinaryTree where

    data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

    unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
    unfold fn seed = case fn seed of
        Nothing -> Leaf
        Just (left, curr, right) -> Node (unfold fn left) curr (unfold fn right)

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild nodes = unfold go 0
        where go i 
                | i == nodes = Nothing 
                |otherwise = Just (i + 1, i, i + 1)
