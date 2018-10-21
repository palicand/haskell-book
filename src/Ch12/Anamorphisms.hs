module Ch12.Anamorphisms where
    myIterate :: (a -> a) -> a -> [a]

    myIterate fn x = x : myIterate fn (fn x)


    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr fn seed = case fn seed of
        Nothing -> []
        Just (curr, next) -> curr : myUnfoldr fn next

    betterIterate :: (a -> a) -> a -> [a]
    betterIterate fn = myUnfoldr (\ a -> Just (a, fn a))