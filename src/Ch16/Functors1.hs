module Ch16.Functors1 where

    data FixMePls a = FixMe | Pls a deriving (Eq, Show)

    instance Functor FixMePls where
        fmap _ FixMe = FixMe
        fmap f (Pls a) = Pls $ f a
    
    replaceWithP :: b -> Char
    replaceWithP = const 'p'

    lms :: [Maybe [Char]]
    lms = [Just "ave", Nothing, Just "woohoo"]

    replaceWithP' :: [Maybe String] -> Char
    replaceWithP' = replaceWithP
    
    liftedReplace  :: Functor f => f a -> f Char
    liftedReplace = fmap replaceWithP

    liftedReplace'  :: [Maybe String] -> String
    liftedReplace' = fmap replaceWithP

    a = fmap (+1) $ read "[1]" :: [Int]
    
    b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

    c = fmap (*2) (\x -> x - 2)
    
    d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer
            changed = fmap ((read . ("123" ++)) . show) ioi
        in fmap (*3) changed
