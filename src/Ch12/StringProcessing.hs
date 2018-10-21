module Ch12.StringProcessing where
    import Data.Maybe
    import Data.List


    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe x = Just x

    replaceThe :: String -> String
    replaceThe "" = ""
    replaceThe x
        | isNothing isThe = "a " ++ replaceThe rest
        | otherwise = head allWords ++ " " ++ replaceThe rest
        where allWords = words x
              isThe = notThe $ head allWords
              rest = unwords $ tail allWords

    

    isWovel :: Char -> Bool
    isWovel ch = ch `elem` "aeiouy"

    startsWithWovel :: String -> Bool
    startsWithWovel [] = False
    startsWithWovel (ch:_) = isWovel ch
    
    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel "" = 0
    countTheBeforeVowel str = case words str of
        "the":w:rest -> if startsWithWovel w then 1 + countTheBeforeVowel (unwords rest) else countTheBeforeVowel $ unwords (w:rest)
        w : rest     ->  countTheBeforeVowel $ unwords rest
    
    countWovels :: String -> Integer
    countWovels = genericLength . filter id . map isWovel
    
    newtype Word' = Word' String deriving (Eq, Show)

    mkWord :: String -> Maybe Word'
    mkWord word = if genericLength word - numWovels > numWovels then Just $ Word' word else Nothing
        where numWovels = countWovels word
