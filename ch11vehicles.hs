module Chapter11Vehicles where


data Price = Price Integer
             deriving (Eq, Show)

data Manufacturer =
              Mini
            | Mazda
            | Tata
            | Honda
              deriving (Eq, Show)

data PlaneSize = PlaneSize Integer deriving (Eq, Show)

data Airline =
              PapuAir
            | CatapultsR'Us
            | TakeYourChancesUnited
              deriving (Eq, Show)

data Vehicle =
              Car Manufacturer Price
            | Plane Airline PlaneSize
              deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Honda (Price 7000)

urCar :: Vehicle
urCar = Car Mazda (Price 8000)

clownCar :: Vehicle
clownCar = Car Tata (Price 2000)

doge :: Vehicle
doge = Plane PapuAir (PlaneSize 10)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True

isPlane _         = False
areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _            = error "doesn't contain manufacturer information"
