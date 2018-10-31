module Ch15.Monoid where
    import Test.QuickCheck


    monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
    monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
    monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidLeftIdentity a = (mempty <> a) == a
    monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
    monoidRightIdentity a = (a <> mempty) == a
    
    data Optional a = Nada | Only a deriving (Eq, Show)

    instance (Semigroup a) => Semigroup (Optional a) where
        (<>) Nada Nada = Nada
        (<>) Nada (Only b) = Only b
        (<>) (Only a) Nada = Only a
        (<>) (Only a) (Only b) = Only $ a <> b

    instance (Monoid a) => Monoid (Optional a) where
        mempty = Nada
    
    newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

    instance Semigroup (First' a) where
        (<>) (First' Nada) b = b
        (<>) (First' (Only a)) _ = (First' (Only a))
    
    instance Monoid (First' a) where
        mempty = First' Nada

    optionalGen :: Arbitrary a => Gen (Optional a)
    optionalGen = 
        frequency [(3, fmap Only arbitrary), (1, return Nada)]

    firstGen :: Arbitrary a => Gen (First' a)
    firstGen =  fmap First' optionalGen

    instance (Arbitrary a) => Arbitrary (First' a) where
        arbitrary = firstGen

    type FirstMappend = First' String -> First' String -> First' String -> Bool
    type FstId = First' String -> Bool
    
    testFirst :: IO ()
    testFirst = do
        quickCheck (monoidAssoc :: FirstMappend)
        quickCheck (monoidLeftIdentity :: FstId)
        quickCheck (monoidRightIdentity :: FstId)
