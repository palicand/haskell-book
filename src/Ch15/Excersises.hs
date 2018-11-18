{-# LANGUAGE DeriveGeneric #-}

module Ch15.Excersises where
    import Test.QuickCheck
    import Data.Monoid
    import GHC.Generics

    -- Trivial
    data Trivial = Trivial deriving (Eq, Show)

    instance Semigroup Trivial where
        a <> _ = a
    
    instance Monoid Trivial where
        mempty = Trivial
    
    instance Arbitrary Trivial where
        arbitrary = return Trivial


    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Property
    -- Identity
    newtype Identity a = Identity a deriving (Eq, Show)

    instance (Semigroup a) => Semigroup (Identity a) where
        (Identity a) <> (Identity b) = Identity $ a <> b

    instance (Arbitrary a) => Arbitrary (Identity  a) where
        arbitrary = fmap Identity arbitrary

    type IdentityAssoc = Identity String -> Identity String -> Identity String -> Property
    
    -- Two
    data Two a b = Two a b deriving (Show, Eq)
    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two a b) <> (Two x y) = Two (a <> x) (b <> y)
    
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b
    
    type TwoAssoc = Two (Sum Integer) String -> Two (Sum Integer) String -> Two (Sum Integer) String -> Property

    -- Three
    data Three a b c = Three a b c deriving (Show, Eq)
    instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
        (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)
    
    instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c
 
    type ThreeAssoc = Three (Sum Integer) String String -> Three (Sum Integer) String String -> Three (Sum Integer) String String -> Property
    
    
    -- BoolConj

    newtype BoolConj = BoolConj Bool deriving (Eq, Show)
    instance Semigroup BoolConj where
        (BoolConj True) <> (BoolConj True) = BoolConj True
        _ <> _ = BoolConj False

    instance Arbitrary BoolConj where
        arbitrary = fmap BoolConj arbitrary

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Property
    

    -- BoolDisj
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        (BoolDisj False) <> (BoolDisj False) = BoolDisj False
        _ <> _ = BoolDisj True

    instance Arbitrary BoolDisj where
        arbitrary = fmap BoolDisj arbitrary
    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Property

    --- OR

    data Or a b = Fst a | Snd b deriving (Eq, Show)

    instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
        (Snd a) <> _ = Snd a
        _ <> Snd b = Snd b
        _ <> b = b

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary]

    type OrAssoc a b = Or a b -> Or a b -> Or a b -> Property

    -- Combine
    newtype Combine a b = Combine { unCombine :: a -> b }

    instance (Semigroup b) => Semigroup (Combine a b) where
        (<>) (Combine f) (Combine g) = Combine $ f <> g

    instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
        mempty = Combine $ const mempty

    

    type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

    combEquality :: (Arbitrary a, Show a, Eq b, Show b) => Combine a b -> Combine a b -> Property
    combEquality (Combine f) (Combine g) = property $ \a -> f a === g a

    semigroupAssoc :: (Eq m, Show m, Semigroup m) => m -> m -> m -> Property
    semigroupAssoc a b c = (a <> (b <> c)) === ((a <> b) <> c)

    combineAssoc :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => CombineAssoc a b
    combineAssoc f g h = ((f <> g) <> h) `combEquality` (f <> (g <> h))

    -- Comp
    newtype Comp a = Comp { unComp :: a -> a } deriving (Generic)
    instance (Semigroup a) => Semigroup (Comp a) where
        (<>) (Comp f) (Comp g) = Comp $ f . g

    instance (Semigroup a, Monoid a) => Monoid (Comp a) where
        mempty = Comp $ const mempty
    
    
    type CompAssoc a = Comp a -> Comp a -> Comp a -> Property
    compEquality :: (Arbitrary a, Show a, Eq a) => Comp a -> Comp a -> Property
    compEquality (Comp f) (Comp g) = property $ \a -> f a === g a

    compAssoc :: (Arbitrary a, Show a, Eq a, Semigroup a) => CompAssoc a
    compAssoc f g h = ((f <> g) <> h) `compEquality` (f <> (g <> h))

    data Validation a b = Failure' a | Success' b deriving (Eq, Show)
    instance Semigroup a => Semigroup (Validation a b) where
        (<>) (Failure' a) (Failure' b) = Failure' $ a <> b
        (<>) (Failure' _) s = s
        (<>) s _ = s
    
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
        arbitrary = oneof [fmap Failure' arbitrary, fmap Success' arbitrary]
    
    type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Property
    

    newtype Mem s a = Mem { runMem :: s -> (a,s) }
    
    instance (Semigroup a) => Semigroup (Mem s a) where
        (<>) (Mem f) (Mem g) = Mem $ \s -> 
            let (fa, fs) = f s
                (ga, gs) = g fs
            in  (fa <> ga, gs)


    instance (Monoid a) => Monoid (Mem s a) where
        mempty = Mem $ \s -> (mempty, s)
    

    testSemiGroup :: IO ()
    testSemiGroup = do
        quickCheck (semigroupAssoc :: TrivialAssoc)
        quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (semigroupAssoc :: ThreeAssoc)
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (semigroupAssoc :: (OrAssoc String String))
        quickCheck (semigroupAssoc :: ValidationAssoc String Int)
        quickCheck $ \(Fn f) (Fn g) (Fn h) -> (combineAssoc :: CombineAssoc String String) (Combine f) (Combine g) (Combine h)
        quickCheck $ \(Fn f) (Fn g) (Fn h) -> (compAssoc :: CompAssoc String) (Comp f) (Comp g) (Comp h)
