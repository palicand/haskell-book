{-# LANGUAGE DeriveGeneric #-}

module Ch15.Excersises where
    import Test.QuickCheck
    import Test.QuickCheck.Function
    import Data.Monoid
    import GHC.Generics

    -- Trivial
    data Trivial = Trivial deriving (Eq, Show)

    instance Semigroup Trivial where
        a <> _ = a
    
    instance Arbitrary Trivial where
        arbitrary = return Trivial

    type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
    -- Identity
    newtype Identity a = Identity a deriving (Eq, Show)

    instance (Semigroup a) => Semigroup (Identity a) where
        (Identity a) <> (Identity b) = Identity $ a <> b

    instance (Arbitrary a) => Arbitrary (Identity  a) where
        arbitrary = fmap Identity arbitrary

    type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
    
    -- Two
    data Two a b = Two a b deriving (Show, Eq)
    instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
        (Two a b) <> (Two x y) = Two (a <> x) (b <> y)
    
    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Two a b
    
    type TwoAssoc = Two (Sum Integer) String -> Two (Sum Integer) String -> Two (Sum Integer) String -> Bool

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
 
    type ThreeAssoc = Three (Sum Integer) String String -> Three (Sum Integer) String String -> Three (Sum Integer) String String -> Bool
    
    
    -- BoolConj

    newtype BoolConj = BoolConj Bool deriving (Eq, Show)
    instance Semigroup BoolConj where
        (BoolConj True) <> (BoolConj True) = BoolConj True
        _ <> _ = BoolConj False

    instance Arbitrary BoolConj where
        arbitrary = fmap BoolConj arbitrary

    type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
    

    -- BoolDisj
    newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

    instance Semigroup BoolDisj where
        (BoolDisj False) <> (BoolDisj False) = BoolDisj False
        _ <> _ = BoolDisj True

    instance Arbitrary BoolDisj where
        arbitrary = fmap BoolDisj arbitrary
    type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

    --- OR

    data Or a b = Fst a | Snd b deriving (Eq, Show)

    instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
        (Snd a) <> _ = Snd a
        _ <> Snd b = Snd b
        _ <> b = b

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
        arbitrary = oneof [fmap Fst arbitrary, fmap Snd arbitrary]

    -- Combine
    newtype Combine a b = Combine { unCombine :: a -> b } deriving (Generic)

    instance (Semigroup b) => Semigroup (Combine a b) where
        (<>) (Combine fn1) (Combine _) = Combine (fn1  <> fn1)
    
    instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
        arbitrary = fmap Combine arbitrary

    instance (Show a, Read a, Show b) => Show (Combine a b) where
        show a = "Combine " ++ show (functionShow $ unCombine a)

    type CombineAssoc = (Combine String String) -> (Combine String String)  -> (Combine String String) -> String -> Bool


    semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
    semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

    combineSemigroupAssoc :: CombineAssoc
    combineSemigroupAssoc a b c str = 
        unCombine (a <> (b <> c)) str == unCombine ((a <> b) <> c) str

    testSemiGroup :: IO ()
    testSemiGroup = do
        quickCheck (semigroupAssoc :: TrivialAssoc)
        quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (semigroupAssoc :: ThreeAssoc)
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (combineSemigroupAssoc :: CombineAssoc)

