module Ch18.Excersises where
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes


    data Nope a = NopeDotJpg deriving (Show, Eq)

    instance Functor Nope where
        fmap _ NopeDotJpg = NopeDotJpg

    instance Applicative Nope where
        pure _ = NopeDotJpg
        (<*>) _ NopeDotJpg = NopeDotJpg

    instance Monad Nope where
        (>>=) _ _ = NopeDotJpg

    instance Arbitrary a => Arbitrary (Nope a) where
        arbitrary = return NopeDotJpg

    instance Eq a => EqProp (Nope a) where
        (=-=) = eq

    testNope :: IO ()
    testNope = do
        let trigger :: Nope (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger

    data PhhhbbtttEither b a =
        Left' a
        | Right' b deriving (Eq, Show)

    instance Functor (PhhhbbtttEither b) where
        fmap f (Left' a) = Left' $ f a
        fmap _ (Right' b) = Right' b

    instance Applicative (PhhhbbtttEither b) where
        pure = Left'

        (<*>) (Left' f) (Left' a) = Left' $ f  a
        (<*>) (Right' f) (Left' _) = Right' f
        (<*>) _ (Right' b) = Right' b

    instance Monad (PhhhbbtttEither b) where
        (>>=) (Left' a) f = f a
        (>>=) (Right' b) _ = Right' b

    instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
        arbitrary = oneof [Left' <$> arbitrary, Right' <$> arbitrary]

    instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where
        (=-=) = eq

    testEither :: IO ()
    testEither = do
        let trigger :: PhhhbbtttEither (Int) (String, Int, String)
            trigger = Left' ("test", 1, "test")
        verboseBatch $ functor trigger
        verboseBatch $ applicative trigger
        verboseBatch $ monad trigger

    newtype Identity a = Identity a deriving (Eq, Ord, Show)

    instance Functor Identity where
        fmap f (Identity a) = Identity $ f a

    instance Applicative Identity where
        pure = Identity
        (<*>) (Identity f) (Identity a) = Identity $ f a

    instance Monad Identity where
        (>>=) (Identity a) f = f a

    instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

    instance Eq a => EqProp (Identity a) where
        (=-=) = eq

    testIdentity :: IO ()
    testIdentity = do
        let trigger :: Identity (String, Int, String)
            trigger = Identity ("test", 1, "test")
        verboseBatch $ functor trigger
        verboseBatch $ applicative trigger
        verboseBatch $ monad trigger


    data List a = Nil | Cons a (List a) deriving (Show, Eq)

    instance Semigroup (List a) where
        (<>) Nil ys = ys
        (<>) (Cons x xs) ys = Cons x $ xs <> ys

    instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons a rest) = Cons (f a) (fmap f rest)

    instance Applicative List where
        pure a = Cons a Nil
        (<*>) Nil _ = Nil
        (<*>) _ Nil = Nil
        (<*>) (Cons f fRest) a = fmap f a <> (fRest <*> a)

    instance Monad List where
        (>>=) Nil _ = Nil
        (>>=) (Cons x xs) f = f x <> (xs >>= f)

    instance (Arbitrary a) => Arbitrary (List a) where
        arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]


    instance Eq a => EqProp (List a) where
        (=-=) = eq

    testList :: IO ()
    testList = do
        let trigger :: List (Int, String, Int)
            trigger = undefined
        verboseBatch $ functor trigger
        verboseBatch $ applicative trigger
        verboseBatch $ monad trigger
    j :: Monad m => m (m a) -> m a
    j m = m >>= id

    l1 :: Monad m => (a -> b) -> m a -> m b
    l1 = fmap

    l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
    l2 f m n =
        (fmap f m) <*> n

    a :: Monad m => m a -> m (a -> b) -> m b
    a m f = f <*> m

    meh :: Monad m => [a] -> (a -> m b) -> m [b]
    meh [] _ = return []
    -- meh (x:xs) f = do
    --     x' <- f x
    --     xs' <- meh xs f
    --     return (x' : xs')
    meh (x:xs) f =
        f x >>= \x' -> meh xs f >>= \xs' -> return (x' : xs')

    flipType :: (Monad m) => [m a] -> m [a]
    flipType xs = meh xs id