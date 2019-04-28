module Ch24.LearnParsers where
    import Text.Trifecta

    stop :: Parser [Char]
    stop = unexpected "stop"

    one :: Parser Char
    one = char '1'

    one' :: Parser [Char]
    one' = one >> stop

    oneTwo = char '1' >> char '2'
    oneTwo' = oneTwo >> stop

    testParse p = print $ parseString p mempty "123"

    pNL s = putStrLn ('\n' : s)

    main = do
        pNL "stop:"
        testParse stop
        pNL "one:"
        testParse one
        pNL "one':"
        testParse one'
        pNL "oneTwo:"
        testParse oneTwo
        pNL "oneTwo':"
        testParse oneTwo'
