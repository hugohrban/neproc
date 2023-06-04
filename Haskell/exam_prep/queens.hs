import Data.List (nub)

queens :: [Int] -> Bool
queens q = checkDiagon coords && checkRows coords
  where
    coords = makeCoords q

makeCoords :: [Int] -> [(Int, Int)]
makeCoords [] = []
makeCoords (x : xs) = (1, x) : rest
  where
    rest = map (\(a, b) -> (a + 1, b)) (makeCoords xs)

nonAttackingDiag :: (Int, Int) -> (Int, Int) -> Bool
nonAttackingDiag (a, b) (c, d) = abs (a - c) /= abs (b - d)

checkDiagon :: [(Int, Int)] -> Bool
checkDiagon [] = True
checkDiagon [x] = True
checkDiagon (x : xs) = all (\q -> nonAttackingDiag q x) xs && checkDiagon xs

checkRows :: [(Int, Int)] -> Bool
checkRows r = length xs == length (nub xs)
  where xs = map snd r
