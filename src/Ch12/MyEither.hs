module Ch12.MyEither where

    lefts' :: [Either a b] -> [a]
    lefts' = foldr f []
        where f (Left a) zs = a : zs
              f _ zs = zs

    rights' :: [Either a b] -> [b]
    rights' = foldr f []
        where f (Right b) zs = b : zs
              f _ zs = zs

    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' = foldr f ([], [])
        where f (Left a) (ys, xs) = (a : ys, xs)
              f (Right b) (ys, xs) = (ys, b : xs) 

    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' fn (Right x) = Just $ fn x
    eitherMaybe' _ _ = Nothing

    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' fn _ (Left a) = fn a
    either' _ fn (Right b) = fn b

    eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe'' fn = either' (const Nothing) (Just . fn)
    