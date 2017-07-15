module Person where

data Gender = Female | Male | Other
  deriving Show

data Ethnicity = Afro | Caucasian | Asiatic | Hispanic | Mixed
  deriving Show

data CivilStatus = Single | Married | Divorced
  deriving Show

data SuperPower = LaserSight
                | FireBeam
                | PsychicControl
                | SuperSpeed
                | SuperStrengh
                | CyborgBody
                | Electromagnetism
  deriving Show

data Human = Human {
    name :: String
  , age :: Int
  , gender :: Gender
  , ethnicity :: Ethnicity
  , heroName :: Maybe String
  , superPower :: Maybe SuperPower
  } deriving Show

data Person = Person {
    self :: Human
  , father :: Human
  , mother :: Human
  , children :: [Human]
  , civilStatus :: CivilStatus
  , couple :: Maybe Human
  } deriving Show


-- | Instances
human1 :: Human
human1 = Human "Blaine" 50 Male Afro (Just "Flash") (Just SuperSpeed)

human2 :: Human
human2 = Human "Martin" 25 Male Caucasian Nothing Nothing

human3 :: Human
human3 = Human "Famike" 47 Female Hispanic (Just "Jean") (Just PsychicControl)

human4 :: Human
human4 = Human "Caroline" 28 Female Mixed Nothing Nothing

human5 :: Human
human5 = Human "James" 28 Female Mixed Nothing Nothing

human6 :: Human
human6 = Human "Diana" 19 Female Mixed Nothing Nothing

human7 :: Human
human7 = Human "Steve" 19 Male Mixed Nothing Nothing

human8 :: Human
human8 = Human "Max" 51 Male Caucasian (Just "Magneto") (Just Electromagnetism)

human9 :: Human
human9 = Human "Janet" 49 Male Hispanic (Just "The Wasp") (Just SuperSpeed)


person1 :: Person
person1 = Person human5 human1 human3 [] Married (Just human4)

person2 :: Person
person2 = Person human4 human8 human9 [] Married (Just human5)

person3 :: Person
person3 = Person human2 human1 human3 [] Single Nothing
