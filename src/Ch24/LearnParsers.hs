module Ch24.LearnParsers where
    import Text.Trifecta
    import Control.Applicative

    stop :: Parser [Char]
    stop = unexpected "stop"

    one :: Parser ()
    one = char '1' >> eof

    one' :: Parser [Char]
    one' = one >> stop

    oneTwo = char '1' >> char '2' >> eof
    oneTwo' = oneTwo >> stop

    p123 :: Parser [Char]
    p123 = do
        (string "123" <|> string "12" <|> string "1")

    testParse :: (Show a) => String -> Parser a -> IO ()
    testParse s p = print $ parseString p mempty s

    string' :: [Char] -> Parser [Char]
    string' str = mapM char str

    pNL s = putStrLn ('\n' : s)

    main = do
        pNL "stop:"
        testParse "123" stop
        pNL "one:"
        testParse "123" one
        pNL "one':"
        testParse "123" one'
        pNL "oneTwo:"
        testParse "123" oneTwo
        pNL "oneTwo':"
        testParse "123" oneTwo'
