module Ch16.TestingFmap where
  import Test.QuickCheck
  import Test.QuickCheck.Function
  import Ch15.Excersises

  functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool

  functorIdentity f = fmap id f == id f

  functorCompose :: (Eq (f c), Functor f) =>  f a -> Fun a b -> Fun b c -> Bool
  functorCompose x (Fun _ f) (Fun _ g) = (fmap g . fmap f $ x) == fmap (g . f) x

  
  instance Functor Identity where

    fmap fn (Identity a) = Identity $ fn a
  
  
  data Pair a = Pair a a deriving (Eq, Show)

  instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

  instance Arbitrary a => Arbitrary (Pair  a) where
      arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

  instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b
  

  instance Functor (Three a b) where
      fmap f (Three a b c) = Three a b $ f c

  data Three' a b = Three' a b b deriving (Eq, Show)
  instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three' a b c

  
  testFunctors :: IO ()
  testFunctors = do
    quickCheck (functorIdentity :: Identity [Int] -> Bool)
    quickCheck (functorCompose :: Identity [Int] -> Fun [Int] String -> Fun String Bool -> Bool)
  
    quickCheck (functorIdentity :: Pair [Int] -> Bool)
    quickCheck (functorCompose :: Pair [Int] -> Fun [Int] String -> Fun String Bool -> Bool)

    quickCheck (functorIdentity :: Two String [Int] -> Bool)
    quickCheck (functorCompose :: Two String [Int] -> Fun [Int] String -> Fun String Bool -> Bool)

    quickCheck (functorIdentity :: Three String [Int] String -> Bool)
    quickCheck (functorCompose :: Three String [Int] String -> Fun String String -> Fun String Bool -> Bool)

    quickCheck (functorIdentity :: Three' [Int] String -> Bool)
    quickCheck (functorCompose :: Three' [Int] String -> Fun String [Int] -> Fun [Int] Bool -> Bool)
