-- 4. úloha
--
-- 1) Implementujte kódování a dekódování RLE (https://en.wikipedia.org/wiki/Run-length_encoding):

-- >>> rleEncode "hello"
-- [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
--
rleEncode :: (Eq a) => [a] -> [(Int, a)]
rleEncode [] = []
rleEncode (x:xs) = (1 + length (takeWhile (x==) xs), x) : rleEncode (dropWhile (x==) xs)

-- >>> rleDecode [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
-- "hello"
--
rleDecode :: [(Int, a)] -> [a]
rleDecode [] = []
rleDecode ((n, ch):xs) = [ch | _ <- [1..n]] ++ rleDecode xs

-- 2) Definujte nekonečný seznam všech prvočísel. Pokuste se o efektivní řešení.
-- Poté pomocí něj definujte funkci, která v daném rozsahu najde dvojici po sobě
-- jdoudích prvočísel s maximálním rozdílem. Pokud je jich více, vrátí první z nich.

-- >>> take 5 primes
-- [2,3,5,7,11]
--

-- O(n^1.5) for `take n primes`
isPrime :: Integer -> Bool
isPrime x = [y | y <- [2..ceilSqrt x], mod x y == 0 ] == []
    where ceilSqrt x = ceiling (sqrt (fromIntegral x))

primes :: [Integer]
primes = 2 : [x | x <- [3..], isPrime x]


-- >>> gap 1000
-- (887, 907)
--

-- finds maximum value in a list of Integers
maxList :: [Integer] -> Integer
maxList [] = undefined
maxList [x] = x
maxList (x:xs) = max x (maxList xs)

-- returns index of maximum
argMax :: [Integer] -> Integer
argMax [] = undefined
argMax [x] = 0
argMax (x:xs)
    | x < maxList xs = 1 + argMax xs
    | otherwise = 0

-- returns list of gaps between every pair of adjancent Integers
gaps :: [Integer] -> [Integer]
gaps xs = [xs !! i - xs !! (i-1) | i <- [1..(length xs - 1)]]

gap :: Integer -> (Integer, Integer)
gap n = 
    (p !! ix_max, p !! (ix_max + 1))
    where
        p = takeWhile (<=n) primes
        ix_max = fromIntegral  (argMax (gaps p))
     

-- Prvním argumentem je konec rozsahu, začátek bude vždy 2. Můžete předpokládat,
-- že konec bude alespoň 3.

-- 3) Implementujte mergesort, který vyhazuje duplikáty.

-- Prvním argumentem je funkce, která provádí porovnávání.
-- Ordering je datový typ, který obsahuje 3 konstanty: LT, EQ, GT
-- (less than, equal, greater than).
--
-- >>> sortWith compare [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (flip compare) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- >>> sortWith compare [1,1,1]
-- [1]

mergeWith :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeWith comp [] ys = ys
mergeWith comp xs [] = xs
mergeWith comp (x:xs) (y:ys)
    | x `comp` y == LT = x : mergeWith comp xs (y:ys)
    | x `comp` y == GT = y : mergeWith comp (x:xs) ys
    | otherwise = x : mergeWith comp xs ys


sortWith  :: (a -> a -> Ordering) -> [a] -> [a]
sortWith comp [] = []
sortWith comp [x] = [x]
sortWith comp xs = 
    mergeWith comp 
    (sortWith comp (take half xs))
    (sortWith comp (drop half xs))
    where 
        half = length xs `div` 2
--
-- BONUS)
--
-- Implementujte následující funkce:

-- combinations n x vygeneruje seznam všech kombinací délky n ze seznamu x.
-- Na pořadí kombinací ve výsledném seznamu nezáleží.
--
-- >>> combinations 2 "abcd"
-- ["ab","ac","ad","bc","bd","cd"]
--

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = [x:cxs | cxs <- combinations (n-1) xs] ++ combinations n xs


-- permutations x vygeneruje seznam všech permutací. Na pořadí permutací ve
-- výsledném seznamu nezáleží.
--
-- >>> permutations "abc"
-- ["abc","bac","bca","acb","cab","cba"]
--

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere x [] = [[x]]
insertEverywhere x xs = [take i xs ++ [x] ++ drop i xs | i <- [0..length xs]]


permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations (x:xs) = concat [insertEverywhere x px | px <- permutations xs]



-- Pomocí těchto funkcí definujte "variace" (občas najdete v české literatuře,
-- v angličtině pro to termín asi neexistuje): kombinace, kde záleží na pořadí
--
-- >>> variations 2 "abc"
-- ["ab","ba","ac","ca","bc","cb"]
--

variations :: Int -> [a] -> [[a]]
variations 0 _ = [[]]
variations _ [] = []
variations n xs = concat [permutations ys | ys <- combinations n xs]
