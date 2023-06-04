import Data.List (group, nub, sort)

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving (Show)

isTaut :: Prop -> Bool
isTaut p = all (\x -> eval x p) (allValues $ varNames p)

-- vyhodnoti vyraz pre nejake nastavenie premennych
eval :: [(Char, Bool)] -> Prop -> Bool
eval xs (Const p) = p
eval xs (Var v) = snd $ head $ filter (\x -> fst x == v) xs
eval xs (Not p) = not $ eval xs p
eval xs (And p1 p2) = eval xs p1 && eval xs p2
eval xs (Or p1 p2) = eval xs p1 || eval xs p2

-- vsetky nazvy premennych z vyroku
varNames :: Prop -> [Char]
varNames (Const c) = []
varNames (Var a) = [a]
varNames (Not p) = varNames p
varNames (And p1 p2) = nub $ varNames p1 ++ varNames p2
varNames (Or p1 p2) = varNames (And p1 p2)

-- vsetky mozne kombinacie ohodnoteni premennych (2^n pre n premennych)
allValues :: [Char] -> [[(Char, Bool)]]
allValues [] = [[]]
allValues [a] = [[(a, True)], [(a, False)]]
allValues (a : as) =
  map (\x -> (a, True) : x) (allValues as)
    ++ map (\x -> (a, False) : x) (allValues as)
