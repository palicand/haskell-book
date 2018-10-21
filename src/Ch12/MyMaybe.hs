module Ch12.MyMaybe where

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _ = False

    isNothing :: Maybe a -> Bool
    isNothing = not . isJust

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee left fn (Just right) = fn right
    mayybee left _ Nothing = left

    fromMaybe :: a -> Maybe a -> a
    fromMaybe defVal = mayybee defVal id

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x : xs) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a) = [a]

    catMaybes :: [Maybe a] -> [a]
    catMaybes = concatMap $ mayybee [] (: [])

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe = foldr f (Just [])
                where f Nothing _ = Nothing
                      f  _  Nothing = Nothing
                      f (Just a) (Just xs) = Just $ a : xs
