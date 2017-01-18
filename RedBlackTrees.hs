data Color = RED | BLACK deriving (Show)
data  RBT a = E | N a Color (RBT a) (RBT a) deriving (Show)

isRed:: (RBT a) -> Color
isRed E = BLACK
isRed (N el color left right) = color

rotateLeft::(RBT a)->(RBT a)
rotateLeft E = E
rotateLeft (N x c l@(N x' BLACK l' r') (N x'' RED l'' r'')) = (N x' c (N x RED l l'') r'')
rotateLeft t = t


rotateRight::(RBT a)->(RBT a)
rotateRight E = E
rotateRight (N x c (N x' RED l'@(N x'' RED l'' r'') r') r) = (N x' c l' (N x RED r' r))
rotateRight t =t

flipColors::RBT a -> RBT a
flipColors E = E
flipColors (N x BLACK l@(N x' RED l' r') r@(N x'' RED l'' r'')) = ( N x RED ( N x' BLACK l' r') (N x'' BLACK l'' r''))
flipColors t =t



insert::(Ord a)=> a->RBT a->RBT a
insert el E = (N el RED E E)
insert el (N x color left right) = flipColors $ rotateRight $ rotateLeft $ case (compare el x) of
        LT -> (N x color (insert el left) right)
        GT -> (N x color left (insert el right))
        EQ -> (N el color left right)

insertFinal:: (Ord a)=> a->RBT a-> RBT a
insertFinal el tree = toBlack(insert el tree)


fromList :: Ord a => [a] -> RBT a
fromList = foldr insertFinal E



pre E = []
pre (N x color l r) =  (pre l) ++ [x] ++  (pre r)


toBlack:: Ord a => RBT a -> RBT a
toBlack E = E
toBlack (N x _ l r) = (N x BLACK l r)