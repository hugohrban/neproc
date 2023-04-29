import Data.Maybe (isJust)
-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Nil | Trie (Maybe v) [(k, Trie k v)]  deriving (Show)
-- Implementujte následující:

empty :: Trie k v
empty = Nil

-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []
--

singleton :: [k] -> v -> Trie k v
singleton [] v = Trie (Just v) []
singleton (k:ks) v = Trie Nothing [(k, singleton ks v)]

-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--

-- returns tuple: 
--      first element is subtrie with key k (there should be only one) or Nil, 
--      second element is list of remaining children (subtries)
childSplit :: (Ord k) => Trie k v -> k -> (Trie k v, [(k, Trie k v)])
childSplit (Trie v ts) k
    | k `notElem` map fst ts = (Nil, ts)
    | otherwise = ( snd $ head $ filter (\x -> fst x == k) ts,
                    filter (\x -> fst x /= k) ts)


insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f [] v (Trie Nothing ts) = Trie (Just v) ts
insertWith f [] v (Trie (Just v') ts) = Trie (Just (f v v')) ts
insertWith f ks v Nil = singleton ks v
insertWith f (k:ks) v (Trie v' ts) = Trie v' ((k, insertWith f ks v child):rest)
    where (child, rest) = childSplit (Trie v' ts) k


-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
--

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert = insertWith const

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find _ Nil = Nothing
find [] (Trie v ts) = v
find (k:ks) (Trie v ts)
    | k `notElem` map fst ts = Nothing
    | otherwise = find ks subtrie
        where subtrie = fst $ childSplit (Trie v ts) k

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member k t = isJust (find k t)

-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'
--
--
-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = foldr (\(k,v) t -> insert k v t) Nil

-- BONUS) Implementujte funkci

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete = undefined

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
