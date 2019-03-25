module Ch20.Excersise where
    import Data.Monoid
    import Test.QuickCheck
    import Test.QuickCheck.Checkers
    import Test.QuickCheck.Classes

    sum' :: (Foldable t, Num a) => t a -> a
    sum' = getSum . foldMap Sum

    product' :: (Foldable t, Num a) => t a -> a
    product' = getProduct . foldMap Product

    elem' :: (Foldable t, Eq a) => a -> t a -> Bool
    elem' x = foldr (\a prev -> x == a || prev) False

    minimum' :: (Foldable t, Ord a) => t a -> Maybe a
    minimum' = foldr (\x y -> Just $ maybe x (min x) y) Nothing

    maximum' :: (Foldable t, Ord a) => t a -> Maybe a
    maximum' = foldr (\x y -> Just $ maybe x (max x) y) Nothing

    null' :: (Foldable t) => t a -> Bool
    null' = foldr (\_ _-> False) True

    length' :: (Foldable t) => t a -> Int
    length' = foldr (\_ y -> y + 1) 0

    fold' :: (Foldable t, Monoid m) => t m -> m
    fold' = foldMap id

    foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
    foldMap' fn = foldr (mappend . fn) mempty


    data Constant a b = Constant b deriving (Eq, Show)
    
    instance Foldable (Constant a) where

        foldMap fn (Constant a) = fn a

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
        arbitrary = Constant <$> arbitrary 



    data Two a b = Two a b deriving (Eq, Show)

    instance Foldable (Two a) where

        foldMap fn (Two _ b) = fn b

    instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = Two <$> arbitrary <*> arbitrary
    
    ch19Test :: IO ()
    ch19Test = do
        let constantT :: Constant Int (Int, Int, String, Int, Bool)
            constantT = undefined
        let twoT :: Two Int (Int, Int, String, Int, Bool)
            twoT = undefined
        quickBatch $ foldable constantT
        quickBatch $ foldable twoT


    filterF :: ( Applicative f , Foldable t , Monoid (f a)) => (a -> Bool) -> t a -> f a
    filterF fn = foldMap (\x -> if fn x then pure x else mempty)
    