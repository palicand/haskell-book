module Ch22.ReadingComprehension where
    import Control.Applicative
    import Data.Maybe
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    newtype HumanName = HumanName String deriving (Eq, Show)
    newtype DogName = DogName String deriving (Eq, Show)
    newtype Address = Address String deriving (Eq, Show)

    data Person = Person { humanName :: HumanName, dogName :: DogName, address :: Address} deriving (Eq, Show)
    
    data Dog = Dog { dogsName :: DogName, dogsAddress :: Address } deriving (Eq, Show)

    getDog :: Person -> Dog
    getDog p = Dog (dogName p) (address p)

    getDogR :: Person -> Dog
    getDogR = Dog <$> dogName <*> address


    newtype Reader r a = Reader { runReader :: r -> a } deriving (Show)


    instance Functor (Reader r) where
        fmap f (Reader ra) = Reader $ (f . ra)
    
    ask :: Reader a a
    ask = Reader id

    myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    myLiftA2 fn a b = fn <$> a <*> b

    asks :: (r -> a) -> Reader r a
    asks = Reader

    instance Applicative (Reader r) where
        
        pure a = Reader $ const a
        (Reader rab) <*> (Reader ra) = Reader $ rab <*> ra


    instance Monad (Reader r) where
        return = pure

        -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
        (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb . ra $ r) r -- ra :: r -> a; aRb :: a -> Reader r b


    instance (Arbitrary r, Arbitrary a, CoArbitrary r) => Arbitrary (Reader r a) where
        arbitrary = Reader <$> arbitrary


    instance (Show a, Arbitrary a, EqProp b) => EqProp (Reader a b) where
        (Reader fn1) =-= (Reader fn2) = fn1 =-= fn2


    getDogRM :: Person -> Dog
    getDogRM = do
        name <- dogName
        addy <- address
        return $ Dog name addy


    getDogRM' :: Reader Person Dog
    getDogRM' = do
        name <- Reader $ \p  -> dogName p
        addy <- Reader $ \p  -> address p
        return $ Dog name addy

    x = [1, 2, 3]
    y = [4, 5, 6]
    z = [7, 8, 9]

    xs :: Maybe Integer
    xs = lookup 3 $ zip x y

    ys :: Maybe Integer
    ys = lookup 6 $ zip y z

    zs :: Maybe Integer
    zs = lookup 4 $ zip x y

    z' :: Integer -> Maybe Integer
    z' n = lookup n $ zip x z

    x1 :: Maybe (Integer, Integer)
    x1 = liftA2 (,) xs ys

    x2 :: Maybe (Integer, Integer)
    x2 = liftA2 (,) ys zs

    x3 :: Integer -> (Maybe Integer, Maybe Integer)
    x3 = liftA2 (,) z' z'

    summed :: Num c => (c, c) -> c
    summed = uncurry (+)

    bolt :: Integer -> Bool
    bolt = liftA2 (&&) (>3) (<8)

    testReader :: IO ()
    testReader = do

        let trigger :: Reader String (String, Bool, [String])
            trigger = undefined

        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger

    sequA :: Integral a => a -> [Bool]
    sequA = sequenceA [(>3), (<8), even]
    
    s' :: Maybe Integer
    s' = summed <$> ((,) <$> xs <*> ys)

    mainExcersise :: IO ()
    mainExcersise = do
        print $ sequenceA [Just 3, Just 2, Just 1]
        print $ sequenceA [x, y]
        print $ sequenceA [xs, ys]
        print $ summed <$> ((,) <$> xs <*> ys)
        print $ fmap summed ((,) <$> xs <*> zs)
        print $ bolt 7
        print $ fmap bolt z
        print $ sequenceA [(>3), (<8), even] 7
