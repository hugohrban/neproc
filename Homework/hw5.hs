import Data.Maybe (isJust, isNothing)
import Control.Monad.Trans.Cont (resetT)
-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Trie (Maybe v) [(k, Trie k v)]  deriving (Show)

-- Implementujte následující:

empty :: Trie k v
empty = Trie Nothing []

isEmpty :: Trie k v -> Bool
isEmpty (Trie v ts)
    | isNothing v && null ts = True
    | otherwise = False

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
--      first element is subtrie with key k (there should be only one) or empty, 
--      second element is list of remaining children (subtries)
childSplit :: (Ord k) => Trie k v -> k -> (Trie k v, [(k, Trie k v)])
childSplit (Trie v ts) k
    | k `notElem` map fst ts = (empty, ts)
    | otherwise = ( snd $ head $ filter (\x -> fst x == k) ts,
                    filter (\x -> fst x /= k) ts)


insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f [] v (Trie Nothing ts) = Trie (Just v) ts
insertWith f [] v (Trie (Just v') ts) = Trie (Just (f v v')) ts
insertWith f (k:ks) v t
    | isEmpty t = singleton (k:ks) v
    | otherwise = Trie v' ((k, insertWith f ks v child):rest)
        where (child, rest) = childSplit t k
              (Trie v' ts) = t


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
find [] (Trie v ts) = v
find (k:ks) (Trie v ts) = find ks subtrie
    where (subtrie, _) = childSplit (Trie v ts) k

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
fromList = foldr (\(k,v) t -> insert k v t) empty

-- BONUS) Implementujte funkci

setToNothing :: (Ord k) => [k] -> Trie k v -> Trie k v
setToNothing [] (Trie v ts) = Trie Nothing ts
setToNothing (k:ks) (Trie v ts)
    | not (member (k:ks) (Trie v ts)) = Trie v ts
    | otherwise = Trie v ((k, setToNothing ks subtrie):rest)
        where (subtrie, rest) = childSplit (Trie v ts) k

getChildren :: (Ord k) => [k] -> Trie k v -> [(k, Trie k v)]
getChildren [] (Trie v ts) = ts
getChildren (k:ks) (Trie v ts) = getChildren ks subtrie
    where (subtrie, _) = childSplit (Trie v ts) k

-- Vymaze hocijaky uzol a vsetko pod nim, bez ohladu na jeho hodnotu
removeNode :: (Ord k) => [k] -> Trie k v -> Trie k v
removeNode [] (Trie v ts) = Trie Nothing []
removeNode [k] (Trie v ts) = Trie v (filter (\x -> fst x /= k) ts)
removeNode (k:ks) (Trie v ts) = Trie v ((k, removeNode ks subtrie):rest)
    where (subtrie, rest) = childSplit (Trie v ts) k

-- Odstraini mrtve vetve pre zadany kluc, ktory musi byt Nothing
removeDeadEnds :: (Ord k) => [k] -> Trie k v -> Trie k v
removeDeadEnds [] t = t
removeDeadEnds ks t
    | member ks t = t
    | not $ null $ getChildren ks t  = t
    | not $ null siblings = removeNode ks t
    | otherwise = removeDeadEnds (init ks) $ removeNode ks t
        where siblings = filter (\x -> fst x /= last ks) $ getChildren (init ks) t

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete ks t = removeDeadEnds ks $ setToNothing ks t

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
