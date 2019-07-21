module Ch24.Fractions where
    import Control.Applicative
    import Data.Ratio ((%))
    import Data.Decimal
    import Text.Trifecta

    data ParsedNumber = ParsedRational {rat :: Rational } | ParsedDecimal {dec :: Decimal} deriving (Eq, Show, Ord)

    badFraction = "1/0"
    alsoBad = "10"
    shouldWork = "1/2"
    shouldAlsoWork = "2/1"

    parseFraction :: Parser Rational
    parseFraction = do
        numerator <- decimal
        _ <- try $ char '/'
        denominator <- decimal
        case denominator of
            0 -> fail "Denominator cannot be a zero"
            _ -> return (numerator % denominator)


    parseDecimals :: Parser Decimal
    parseDecimals = do
        before <- decimal
        after <- option 0 $ try $ char '.' >> decimal
        let decPlaces = (length $ show after)
            fullNum = (read (show before ++ show after))
        return $ Decimal (fromIntegral decPlaces) fullNum


    parseAll :: Parser ParsedNumber
    parseAll = try (ParsedRational <$> parseFraction) <|> ParsedDecimal <$> parseDecimals
        

    main :: IO ()
    main = do
        let parseFraction' = parseString parseFraction mempty
        print $ parseFraction' shouldWork
        print $ parseFraction' shouldAlsoWork
        print $ parseFraction' alsoBad
        print $ parseFraction' badFraction