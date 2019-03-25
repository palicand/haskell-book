module Ch19.Reader where
    import Control.Applicative
    import Data.Char

    boop = (*2)
    doop = (+10)
    
    bip :: Integer -> Integer
    bip = boop . doop

    bbop :: Integer -> Integer
    bbop = (+) <$> boop <*> doop
    
    duwop :: Integer -> Integer
    duwop = liftA2 (+) boop doop

    cap :: [Char] -> [Char]
    cap xs = map toUpper xs

    rev :: [Char] -> [Char]
    rev xs = reverse xs
