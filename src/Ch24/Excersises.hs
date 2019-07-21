module Ch24.Excersises where

    import Data.Char
    import Control.Applicative
    import Text.Trifecta

    data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)

    type Major = Integer
    type Minor = Integer
    type Patch = Integer
    type Release = [NumberOrString]
    type Metadata = [NumberOrString]

    data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show)


    parseNOS :: Parser NumberOrString
    parseNOS = (NOSS <$> some letter) <|> (NOSI <$> integer)

    parseSemVer :: Parser SemVer
    parseSemVer = do
        major <- integer
        _ <- char '.'
        minor <- integer
        _ <- char '.'
        patch <- integer
        rel <- option [] $ try (char '-' >> sepBy parseNOS (char '.'))
        meta <- option [] $ try (char '+' >> sepBy parseNOS (char '.'))
        eof
        return $ SemVer major minor patch rel meta


    instance Ord NumberOrString where

        a <= b = a == b || a < b
        (NOSI a) < (NOSI b) = a < b
        (NOSS a) < (NOSS b) = a < b
        (NOSS _) < _ = False
        (NOSI _) < _ = True

    instance Eq SemVer where
        (SemVer maj1 min1 p1 rel1 _) == (SemVer maj2 min2 p2 rel2 _) =
            maj1 == maj2 && min1 == min2 && p1 == p2 && rel1 == rel2



    instance Ord SemVer where

        sv1 <= sv2 = sv1 == sv2 || sv1 < sv2

        (SemVer maj1 min1 p1 rel1 _) < (SemVer maj2 min2 p2 rel2 _) =
            maj1 <= maj2 && min1 <= min2 && p1 <= p2 && (rel2 == [] || rel1 < rel2)


    ps = parseString
    psv = ps parseSemVer mempty


    parseDigit :: Parser Char
    parseDigit = oneOf "1234567890" <|> fail "expected: an integer"

    base10Integer :: Parser Integer
    base10Integer = pd $ some parseDigit
                        where pd :: Parser [Char] -> Parser Integer
                              pd pch =
                                foldl (\b a -> toInteger (digitToInt a) + b * 10) 0 <$> pch
    
    signedInteger :: Parser Integer
    signedInteger = do
        sign <- (\x -> if x == '+' then 1 else -1) <$> (option '+' $ try (oneOf "+-"))
        parsedInteger <- base10Integer
        return (sign * parsedInteger)