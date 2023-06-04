import Data.Foldable (minimumBy)
import Data.Function

-- Na vstupu je úplný graf G, který má vrcholy v nějakých bodech v rovině, tj. určených dvojicí souřadnic. Dále je určen jeden vrchol x grafu G jako počáteční. Délky hran se počítají dynamicky z polohy koncových bodů hrany pomocí předaného funkcionálního parametru vzd.

-- Chcete vytvořit proceduru odhadHam, která heuristicky spočítá odhad nejkratší hamiltonovské cesty začínající v x. Výstup je seznam vrcholů na nalezené cestě. Použitá heuristika je, že najdeme nejbližší dosud nepoužitý vrchol k aktuálnímu konci cesty a přidáme ho k cestě (hamiltonovská cesta prochází všemi vrcholy grafu a v našem případě začíná v x.)

-- a) Navrhněte reprezentaci grafu G, ve kterém vrcholy mají jméno a polohu v rovině.

-- b) Napište typ funkce odhadHam, pokud očekávané volání je
-- KÓD: VYBRAT VŠE
-- odhadHam vzd graf x
-- c) Napište funkci odhadHam.

-- ------ Types ------
-- type Point = (Int, Int)

-- type Graph = [Point]

-- ------ Norms ------
-- vzd :: Point -> Point -> Double
-- vzd (x1, y1) (x2, y2) = sqrt (fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2))

-- manhattan :: Point -> Point -> Double
-- manhattan (x1, y1) (x2, y2) = fromIntegral (abs (x1 - x2) + abs (y1 - y2))

-- supremum :: Point -> Point -> Double
-- supremum (x1, y1) (x2, y2) = fromIntegral (max (abs (x1 - x2)) (abs (y1 - y2)))

-- ------ Hamilton ------
-- odhadHam :: (Point -> Point -> Double) -> Graph -> Point -> [Point]
-- odhadHam heur graph originPt = reverse $ odhadHam' heur graph originPt []

-- odhadHam' :: (Point -> Point -> Double) -> Graph -> Point -> [Point] -> [Point]
-- odhadHam' heur graph originPoint visited
--   | null graph = visited
--   | otherwise = odhadHam' heur g np (np : visited)
--   where
--     np = nextPoint heur graph originPoint
--     g = filter (/= np) graph

-- nextPoint :: (Point -> Point -> Double) -> Graph -> Point -> Point
-- nextPoint heur graph originPoint = minimumBy (compare `on` (heur originPoint)) graph


------ Types ------
type Point t = (t, Int, Int)

type Graph t = [Point t]

------ Norms ------
vzd :: Point t -> Point t -> Double
vzd (_, x1, y1) (_, x2, y2) = sqrt (fromIntegral ((x1 - x2) ^ 2 + (y1 - y2) ^ 2))

manhattan :: Point t -> Point t -> Double
manhattan (_, x1, y1) (_, x2, y2) = fromIntegral (abs (x1 - x2) + abs (y1 - y2))

supremum :: Point t -> Point t -> Double
supremum (_, x1, y1) (_, x2, y2) = fromIntegral (max (abs (x1 - x2)) (abs (y1 - y2)))

------ Hamilton ------
odhadHam :: Eq t => (Point t -> Point t -> Double) -> Graph t -> Point t -> [Point t]
odhadHam heur graph originPt = reverse $ odhadHam' heur graph originPt []

odhadHam' :: Eq t => (Point t -> Point t -> Double) -> Graph t -> Point t -> [Point t] -> [Point t]
odhadHam' heur graph originPoint visited
  | null graph = visited
  | otherwise = odhadHam' heur g np (np : visited)
  where
    np = nextPoint heur graph originPoint
    g = filter (/= np) graph

nextPoint :: (Point t -> Point t -> Double) -> Graph t -> Point t -> Point t
nextPoint heur graph originPoint = minimumBy (compare `on` heur originPoint) graph

isPrime :: Int -> Bool
isPrime n = all (\x -> mod n x /= 0) (takeWhile (\x -> x*x <= n) [2..])

-- isPrime :: Int -> Bool
-- isPrime n = all (\x -> mod n x /= 0) [2..(n-1)]

primes :: [Int]
primes = filter isPrime [2..]

main :: IO ()
main = do
    print $ primes!!100_000
    return ()
