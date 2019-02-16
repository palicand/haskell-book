module Ch17.Applicative where

    added :: Maybe Integer
    added = (+3) <$> lookup 3 (zip [2, 3, 4] [4, 5, 6])

    y :: Maybe Integer
    y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

    tupled :: Maybe (Integer, Integer)
    tupled = (,) <$> y <*> z

    validateLength :: Int -> String -> Maybe String
    validateLength maxLen s =
        if length s > maxLen then Nothing
        else Just s

    newtype Name = Name String deriving (Eq, Show)
    newtype Address = Address String deriving (Eq, Show)

    mkName :: String -> Maybe Name
    mkName s = fmap Name $ validateLength 25 s

    mkAddress :: String -> Maybe Address
    mkAddress s = fmap Address $ validateLength 100 s

    data Person = Person Name Address deriving (Eq, Show)

    mkPerson :: String -> String -> Maybe Person
    mkPerson n a = Person <$> mkName n <*> mkAddress a 
    