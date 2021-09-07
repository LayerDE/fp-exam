import Hoopl.Graph (NonLocal(successors))
data Currency = USD | EUR | AFN  | ALL | AOA deriving (Enum)

nextCurrency ::  String -> Currency
nextCurrency s
    |s == "US Dollar" = succ USD
    |s == "Euro" = succ EUR
    |s == "Afghan Afghani" = succ AFN
    |s == "Albanian Lek" = succ ALL
    |s == "Angholan Kwanza" = succ AOA



data Point2D = Cartesian Double Double | Polar Double Double

size ::  Point2D -> Double
size (Cartesian x y) = sqrt (x^2+y^2)
size (Polar x r) = x

