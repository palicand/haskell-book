module Ch11.Phone where
    import Data.List
    import Data.Char
    import Data.Maybe


    newtype DaPhone = DaPhone [(Char, (Digit, Presses))]

    convo :: [String]
    convo = ["Wanna play 20 questions",
             "Ya",
             "U 1st haha",
             "Lol ok. Have u ever tasted alcohol",
             "Lol ya",
             "Wow ur cool haha. Ur turn",
             "Ok. Do u think I am pretty Lol",
             "Lol ya",
             "Just making sure rofl ur turn"]
    -- validButtons = "1234567890*#"
    type Digit = Char
    -- Valid presses: 1 and up
    type Presses = Int

    type Label = String

    keyboard :: [(Digit, Label)]
    keyboard = [('1', ""), ('2', "abc"), ('3', "def"), ('4', "ghi"), ('5', "jkl"), ('6', "mno"), ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"), ('*', ""), ('0', "+  _"), ('#', ".,")]

    invertKeyboard :: [(Digit, Label)] -> [(Char, (Digit, Presses))]
    invertKeyboard = concatMap (\x@(digit, label) -> map (\(ch, idx) -> (ch, (digit, idx + 1))) $ zip label [0..] )

    phone = DaPhone $ invertKeyboard keyboard

    reverseTaps :: DaPhone
                -> Char
                -> [(Digit, Presses)]
    reverseTaps phone@(DaPhone keyboard) ch
        | isUpper ch = ('*', 1) : reverseTaps phone (toLower ch)
        | otherwise = maybeToList $ lookup ch keyboard
    
    -- assuming the default phone definition
    -- 'a' -> [('2', 1)]
    -- 'A' -> [('*', 1), ('2', 1)]
    cellPhonesDead :: DaPhone
                   -> String
                   -> [(Digit, Presses)]
    cellPhonesDead phone = concatMap (reverseTaps phone)

    fingerTaps :: [(Digit, Presses)] -> Presses
    fingerTaps = sum . map snd

