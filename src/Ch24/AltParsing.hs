{-# LANGUAGE QuasiQuotes #-}

module Ch24.AltParsing where
    import Control.Applicative
    import Text.RawString.QQ
    import Text.Trifecta


    type NumberOrString = Either Integer String

    eitherOr :: String
    eitherOr = [r|
123
abc
456
def
|]

    parseNos :: Parser NumberOrString
    parseNos = do
        skipMany (oneOf "\n")
        v <- (Left <$> integer) <|> (Right <$> some letter)
        skipMany (oneOf "\n")
        return v

    main = do
        let p f = parseString f mempty
        print $ p (some parseNos) eitherOr