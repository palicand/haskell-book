module Ch12.Person where


    type Name = String
    type Age = Integer
    data Person = Person Name Age deriving Show
    data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)
    type ValidatePerson a =
        Either [PersonInvalid] a

    
    ageOkay :: Age -> Either [PersonInvalid] Age
    ageOkay age = if age >= 0
        then Right age
        else Left [AgeTooLow]

    nameOkay :: Name -> Either [PersonInvalid] Name
    nameOkay name = if name /= ""
        then Right name
        else Left [NameEmpty]
    

    mkPerson :: Name -> Age -> ValidatePerson Person
    mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

    mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson' (Right name) (Right age) = Right $ Person name age
    mkPerson' (Left name) (Left age) = Left (name ++ age)
    mkPerson' (Left name) _ = Left name
    mkPerson' _ (Left age) = Left age