module Ch23.Excersises where
    import Ch23.MyState

    get' :: MyState s s
    get' = MyState $ \x -> (x, x)

    put' :: s -> MyState s ()
    put' s = MyState $ \_ -> ((), s)

    exec :: MyState s a -> s -> s
    exec (MyState sa) s = snd $ sa s

    eval :: MyState s a -> s -> a
    eval (MyState sa) s = fst $ sa s

    modify :: (s -> s) -> MyState s ()
    modify fn = MyState $ \s -> ((), fn s) 