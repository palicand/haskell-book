module Ch21.SkiFree where
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    data S n a = S (n a) a deriving (Eq, Show)

    instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
        arbitrary = S <$> arbitrary <*> arbitrary

    instance (Eq (n a), Eq a) => EqProp (S n a) where
        (=-=) = eq

    instance Functor n => Functor (S n) where
        fmap f (S n a) = S (fmap f n) (f a)

    instance Foldable n => Foldable (S n) where
        foldMap f (S a b) = foldMap f a <> f b

    instance Traversable n => Traversable (S n) where
        sequenceA (S a b) = S <$> sequenceA a <*> b


    testSkiFree :: IO ()
    testSkiFree = do
        let trigger :: S [] (Int, String, String)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ traversable trigger