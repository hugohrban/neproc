---------- mine

import Data.List

data Tree t = Tree [(t, t)] deriving (Show)

vrstvy :: Ord t => Tree t -> [[t]]
vrstvy (Tree []) = []
vrstvy (Tree t) = case listy of
  [] -> error ""
  _ -> listy : vrstvy (Tree removed)
  where
    listy = getLeaves t
    removed = filter (\(a, b) -> a `notElem` listy && b `notElem` listy) t

getLeaves :: Ord t => [(t, t)] -> [t]
getLeaves [] = []
getLeaves q =
  concat $
    filter (\x -> length x == 1) $
      group $
        sort $
          concatMap (\(a, b) -> [a, b]) q

-------- vit

type Graph a = [(a, a)]

layers :: (Ord a) => Graph a -> [[a]]
layers [] = []
layers g = case leaves of
  [] -> error "layers: cyclic graph"
  _ -> leaves : layers removed
  where
    leaves = concat . filter ((== 1) . length) . group . sort . concatMap (\(a, b) -> [a, b]) $ g
    removed = filter (\(a, b) -> not (a `elem` leaves || b `elem` leaves)) g