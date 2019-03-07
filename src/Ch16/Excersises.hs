{-# LANGUAGE FlexibleInstances #-}

module Ch16.Excersises where
  import Ch16.TestingFmap (functorIdentity, functorCompose)
  import Test.QuickCheck

  data Sum b a = First a | Second b

  instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

  data Company a c b =
    DeepBlue a c
    | Something b

  instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

  data More b a =
      L a b a
      | R b a b
      deriving (Eq, Show)

  instance Functor (More x) where
      fmap f (L a b a') = L (f a) b (f a')
      fmap f (R b a b') = R b (f a) b'

  data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

  instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary =
      oneof [elements [Finance], fmap Desk arbitrary, fmap Bloor arbitrary]

  data K a b = K a deriving (Eq, Show)

  instance Functor (K a) where
    fmap _ (K a) = K a

  instance (Arbitrary a) => Arbitrary (K a b) where
    arbitrary = fmap K arbitrary


  newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

  instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip (K $ f a)


  instance (Arbitrary b) => Arbitrary (Flip K a b) where
    arbitrary = fmap Flip arbitrary
  

  ch16ExcersiseTests :: IO ()
  ch16ExcersiseTests = do
    quickCheck (functorIdentity :: Quant [Int] [String] -> Bool)
    quickCheck (functorCompose :: Quant [Int] [String] -> Fun [String] [Int] -> Fun [Int] Bool -> Bool)

    quickCheck (functorIdentity :: K [Int] [String] -> Bool)
    quickCheck (functorCompose :: K [Int] [String] -> Fun [String] [Int] -> Fun [Int] Bool -> Bool)

    quickCheck (functorIdentity :: Flip K [Int] [String] -> Bool)
    quickCheck (functorCompose :: Flip K [Int] [String] -> Fun [String] [Int] -> Fun [Int] Bool -> Bool)
