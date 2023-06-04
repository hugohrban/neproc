data Tree = Nil | Node Tree Int Tree
  deriving (Show, Eq, Ord)

allBalanced :: Int -> [Tree]
allBalanced n = map label $ allBST n

allBST :: Int -> [Tree]
allBST 0 = [Nil]
allBST 1 = [Node Nil 0 Nil]
allBST n
  | odd n = combine half half
  | even n = combine half biggerHalf ++ combine biggerHalf half
  where
    half = allBST $ (n - 1) `div` 2
    biggerHalf = allBST $ (n + 1) `div` 2

combine :: [Tree] -> [Tree] -> [Tree]
combine l [] = [Node t 0 Nil | t <- l]
combine [] r = [Node Nil 0 t | t <- r]
combine l r = [Node lt 0 rt | lt <- l, rt <- r]

getSize :: Tree -> Int
getSize Nil = 0
getSize (Node l _ r) = getSize l + 1 + getSize r

label :: Tree -> Tree
label = labelWithoffset 0

labelWithoffset :: Int -> Tree -> Tree
labelWithoffset _ Nil = Nil
labelWithoffset offset (Node l _ r) =
  Node
    (labelWithoffset offset l)
    (getSize l + offset + 1)
    (labelWithoffset (offset + getSize l + 1) r)