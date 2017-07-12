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
human4 = Human "Caroline" 28 Female Asiatic Nothing Nothing

human5 :: Human
human5 = Human "James" 28 Female Mixed (Just "Cyclops") (Just LaserSight)

person1 :: Person
person1 = Person human5 human1 human3 [] Single Nothing
