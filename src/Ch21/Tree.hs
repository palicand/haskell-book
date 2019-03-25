module Ch21.Tree where
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

    instance Foldable Tree where
        foldMap _ Empty = mempty
        foldMap f (Leaf a) = f a
        foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

    instance Functor Tree where
        fmap _ Empty = Empty
        fmap f (Leaf a) = Leaf $ f a
        fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

    instance Traversable Tree where
        traverse _ Empty = pure Empty
        traverse f (Leaf a) = Leaf <$> f a
        traverse f (Node l a r) = Node <$> (traverse f l) <*> (f a) <*> (traverse f r)

    instance (Eq a) => EqProp (Tree a) where
        (=-=) = eq 

    instance (Arbitrary a) => Arbitrary (Tree a) where
        arbitrary = oneof [return Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]
    
    testTree :: IO ()
    testTree = do
        let trigger :: Tree (Int, String, String)
            trigger = undefined
        let foldT :: Tree (Int, String, String, Int, String)
            foldT = undefined
        quickBatch $ functor trigger
        quickBatch $ foldable foldT
        quickBatch $ traversable trigger