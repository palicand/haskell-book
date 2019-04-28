module Ch23.MyState where
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    newtype MyState s a = MyState {runMyState :: s -> (a, s)} deriving (Show)

    instance Functor (MyState s) where
        fmap f (MyState g) = MyState $ \x ->
            let (a, s) = g x
            in  (f a, s)

    instance (Monoid s) => Applicative (MyState s) where
        pure a = MyState $ \s -> (a, s)
        (MyState f) <*> (MyState g) = MyState $ \x ->
            let (fAp, fs) = f x
                (a, gs) = g fs
            in  (fAp a, gs)

    instance (Monoid s) => Monad (MyState s) where
        (MyState fs) >>= f =
            MyState $ \x ->
                let (a, s) = fs x
                    (MyState g) = f a
                in g s

    instance (Arbitrary s, Arbitrary a, CoArbitrary s) => Arbitrary (MyState s a) where
        arbitrary = MyState <$> arbitrary
    
    instance (Show s, Arbitrary s, EqProp a, EqProp s) => EqProp (MyState s a) where
        (MyState f) =-= (MyState g) = f =-= g
    
    
    testState :: IO ()
    testState = do
        let trigger :: MyState String (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
    