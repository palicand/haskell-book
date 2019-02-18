module Ch18.AlternativeEither where
    
    data Sum a b = First a | Second b deriving (Eq, Show)

    instance Functor (Sum a) where

        fmap _ (First a) = First a
        fmap f (Second a) = Second $ f a


    instance Applicative (Sum a) where

        pure = Second
        (<*>) (First f) _ = First f
        (<*>) _ (First a) = First a
        (<*>) (Second f) (Second a) = Second $ f a

    instance Monad (Sum a) where
        (>>=) (First a) _ = First a
        (>>=) (Second a) f = f a