--ex3.1
data Currency = USD | EUR | AFN  | ALL | AOA deriving (Enum)

nextCurrency ::  String -> Currency
nextCurrency s
    |s == "US Dollar" = succ USD
    |s == "Euro" = succ EUR
    |s == "Afghan Afghani" = succ AFN
    |s == "Albanian Lek" = succ ALL
    |s == "Angholan Kwanza" = succ AOA


--ex3.2
data Point2D = Cartesian Double Double | Polar Double Double

size ::  Point2D -> Double
size (Cartesian x y) = sqrt (x^2+y^2)
size (Polar x r) = x

--ex3.3 
data Prop atom = A atom
               | Conjunction (Prop atom) (Prop atom)
               | Neg (Prop atom)
               | FFalse
    deriving (Eq, Ord, Show)

holds :: Eq atom => Prop atom -> [atom] -> Bool
holds (Conjunction l r ) lst = (holds l lst) && (holds r lst)
holds (A atom) l  = atom `elem` l
holds FFalse _    = False
holds (Neg p) l   = not $ holds p l