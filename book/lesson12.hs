import Control.Arrow (Arrow (first))
import Control.Monad.RWS (First)

type PatientName = (String, String)

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

data Sex = Male | Female deriving (Show)

data RhType = Pos | Neg deriving (Show)

data ABOType = A | B | AB | O deriving (Show)

data BloodType = BloodType ABOType RhType deriving (Show)

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateToV2 :: Patient -> Patient -> Bool
canDonateToV2 p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  deriving (Show)

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) =
  f ++ " " ++ m ++ " " ++ l

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

patientSummary :: Patient -> String
patientSummary patient = 
    "*****************\n" ++
    "Patient Name: " ++ showName (name patient) ++ "\n" ++
    "Sex: " ++ showSex (sex patient) ++ "\n" ++
    "Age: " ++ show (age patient) ++ "\n" ++
    "Weight: " ++ show (weight patient) ++ "\n" ++
    "Height: " ++ show (height patient) ++  "\n" ++
    "Blood Type: " ++ showBloodType (bloodType patient)


data Sample = Sample {
    patient :: Patient,
    createdAt :: Int
}

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

patientInfo :: PatientName -> Age -> Height -> String
patientInfo patient age height = name ++ " " ++ ageHeight
  where
    name = snd patient ++ ", " ++ fst patient
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"