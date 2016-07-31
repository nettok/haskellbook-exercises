module Chapter11Programming where


data OperatingSystem =
    GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show, Enum)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum)

data Programmer =
  Programmer {
    os   :: OperatingSystem
  , lang :: ProgrammingLanguage
  } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = enumFrom $ toEnum 0

allLanguages :: [ProgrammingLanguage]
allLanguages = enumFrom $ toEnum 0

allProgrammers :: [Programmer]
allProgrammers = [Programmer os' lang' | os' <- allOperatingSystems, lang' <- allLanguages]
