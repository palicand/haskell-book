module Ch18.EitherMonad where

    type Founded = Int
    type Coders = Int

    data SoftwareShop = Shop {
        founded :: Founded,
        coders  :: Coders
    } deriving (Eq, Show)

    data FoundedError =
        NegativeYears Founded |
        TooManyYears Founded |
        NegativeCoders Coders |
        TooManyCoders Coders |
        TooManyCodersForYears Founded Coders
        deriving (Eq, Show)

    validateFounded :: Founded -> Either FoundedError Founded
    validateFounded n
        | n < 0 = Left $ NegativeYears n
        | n > 500 = Left $ TooManyYears n
        | otherwise = Right n

    validateCoders :: Coders -> Either FoundedError Coders
    validateCoders c
        | c < 0 = Left $ NegativeCoders c
        | c > 5000 = Left $ TooManyCoders c
        | otherwise = Right c

    mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
    mkSoftware years programmers = do
        founded' <- validateFounded years
        coders' <- validateCoders programmers
        if coders' > founded' `div` 10
            then Left $ TooManyCodersForYears founded' coders'
            else Right $ Shop founded' coders'
