module Ch21.Excersises where

    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    newtype Identity a = Identity a deriving (Eq, Ord, Show)

    instance Foldable Identity where
        foldMap f (Identity a) = f a

    instance Functor Identity where
        fmap f (Identity a) = Identity $ f a

    instance Traversable Identity where
        sequenceA (Identity a) = Identity <$> a

    instance (Arbitrary a) => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

    instance Eq a => EqProp (Identity a) where
        (=-=) = eq

    newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)
    
    instance Foldable (Constant a) where
        foldMap _ _ = mempty

    instance Functor (Constant a) where
        fmap _ (Constant a) = Constant a

    instance Traversable (Constant a) where
        sequenceA (Constant a) = pure $ Constant a

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b)  where
        arbitrary = Constant <$> arbitrary

    instance Eq a => EqProp (Constant a b) where
        (=-=) = eq


    data Optional a = Nada | Yep a deriving (Eq, Show)

    instance Foldable Optional where
        foldMap _ Nada = mempty
        foldMap f (Yep a) = f a

    instance Functor Optional where
        fmap _ Nada = Nada
        fmap f (Yep a) = Yep $ f a

    instance Traversable Optional where
        sequenceA Nada = pure Nada
        sequenceA (Yep a) = Yep <$> a

    instance (Arbitrary a) => Arbitrary (Optional a)  where
        arbitrary = oneof [Yep <$> arbitrary, return Nada]

    instance Eq a => EqProp (Optional a) where
        (=-=) = eq


    data List a = Nil | Cons a (List a) deriving (Eq, Show)

    instance Foldable List where
        foldMap _ Nil = mempty
        foldMap f (Cons x xs) = f x <> foldMap f xs

    instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons x xs) = Cons (f x) (fmap f xs)

    instance Traversable List where
        sequenceA Nil = pure Nil
        sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

    instance (Arbitrary a) => Arbitrary (List a)  where
        arbitrary = oneof [Cons <$> arbitrary <*> arbitrary, return Nil]

    instance Eq a => EqProp (List a) where
        (=-=) = eq

    data Big a b = Big a b b deriving (Eq, Show)

    instance Functor (Big a) where
        fmap f (Big a x y) = Big a (f x) (f y)

    instance Foldable (Big a) where
        foldMap f (Big _ x y) = (f x) <> (f y)

    instance Traversable (Big a) where
        sequenceA (Big a x y) = Big <$> pure a <*> x <*> y

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
        arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary
 
    instance (Eq a, Eq b) => EqProp (Big a b) where
        (=-=) = eq

    ch21Test :: IO ()
    ch21Test = do
        let identityT :: Identity (String, Bool, [String])
            identityT = undefined
        let constantT :: Constant Int (String, Bool, [String])
            constantT = undefined
        let optionalT :: Optional (String, Bool, [String])
            optionalT = undefined
        let listT :: List (String, Bool, [String])
            listT = undefined
        let bigT :: Big Int (String, Bool, [String])
            bigT = undefined
        quickBatch $ traversable identityT
        quickBatch $ traversable constantT
        quickBatch $ traversable optionalT
        quickBatch $ traversable listT
        quickBatch $ traversable bigT
