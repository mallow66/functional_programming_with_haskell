-- Binary search tree

data BT a = E | N a (BT a) (BT a)

instance (Show a) => Show (BT a) where
    show E = ""
    show (N x l r) = "   ("++show x++")   " ++ "["++ show l ++"]"++" | "++"["++show r++"]"

root:: (BT a)->a
root E = error "Empty node"
root (N x _ _) = x

left :: (BT a)->a
left (N _ l _) = root l

right :: (BT a)-> a
right (N _ _ r) = root r

newNode:: a -> (BT a)
newNode x = (N x E E)

add::(Ord a)=>a->(BT a)->(BT a)
add el E = newNode el
add el t@(N x l r) | el < x = (N x (add el l) r)
                   | el == x = t
                   |otherwise = (N x l (add el r))


search :: (Ord a)=> a->(BT a)->Bool
search _ E = False
search el (N x l r) | el == x = True
                    | el < x = search el l
                    |otherwise = search el r


prefix :: (BT a)->[a]
prefix E = []
prefix (N x l r ) = [x]++ prefix l ++ prefix r

postfix :: (BT a)-> [a]
postfix E = []
postfix (N x l r) = postfix l ++ postfix r ++ [x]


infixe :: ( BT a ) -> [a]
infixe E = []
infixe  (N x l r ) = infixe l ++ [x] ++ infixe r

height ::(Ord a)=> (BT a) -> Int
height E  = 0
height (N  _ l r ) = 1 + max (height l) (height r)

fromList :: (Ord a) => [a] -> ( BT a )
fromList [] = E
fromList (x:xs) = add x (fromList xs)


fromList' :: (Ord a) => [a] -> ( BT a )
fromList' xs = foldr(add) E xs

min' ::(Ord a) => (BT a) -> a
min' E = error"Empty Tree"
min' (N x E _) = x
min' (N _ l _) = min' l


delete :: (Ord a) => a -> (BT a) -> ( BT a )
delete _ E = error"the tree is empty ! "
delete y (N x l E) | x == y  = l
delete y (N x E r) | x== y = r
delete y (N x l r )| y < x = (N x (delete y l) r)
                   | y > x = (N x l (delete y r))
                   | otherwise = (N z l (delete z r))
                     where z = min' r
