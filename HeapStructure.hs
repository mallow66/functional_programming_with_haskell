module HeapStructure where
-- Leftist Heap data structure


type Rang = Int
data LH a = EH | H Rang a ( LH a ) ( LH a ) deriving (Show)

distance :: (LH a) -> Rang
distance EH = 0
distance (H r _ _ _ ) = r


-- merge two heaps
merge :: (Ord a) => (LH a) ->(LH a) -> (LH a)
merge EH t = t
merge t EH = t
merge t1@(H r  x a1 a2) t2@(H r' x'  b1 b2) | x <= x' = join x  a1 (merge a2 t2)
                                          | otherwise = join x' b1 ( merge t1  b2 )

join :: (Ord a) => a -> (LH a) -> (LH a) -> (LH a)
join x a b | distance a >= distance b = (H ((distance b)+1) x b a )
           | otherwise  = (H ((distance a)+1) x a b )


add :: (Ord a) => a -> (LH a) -> (LH a)
add el heap = merge (H 1 el EH EH) heap

delete :: (Ord a) => (LH a) -> (LH a)
delete EH = error"Empty heap"
delete (H _ _ l r) = merge l r

minimum' :: (LH a) -> a
minimum' EH = error"Empty heap"
minimum' (H _ x _ _ ) = x

fromList ::(Ord a)=> [a] ->(LH a)
fromList [] = EH
fromList (x:xs) = add x (fromList xs)

toList ::(Ord a)=> (LH a) -> [a]
toList EH = []
toList t@(H _ x l r) = x : toList ( delete t )

heapSort :: (Ord a)=> [a] -> [a]
heapSort xs = toList (fromList xs)

size::LH a-> Int
size EH = 0
size ( H _ _ l r) = 1 + (size l) + (size r)

--here we gonna take two minimum of the heap and it rest
getTwoMin :: (Ord a) => (LH a) -> (a, a, LH a)
getTwoMin EH = error"Empty heap"
getTwoMin t@(H _ x l r ) = (x, m2, rest)
                            where
                                m2 = minimum' (delete t)
                                rest = delete (delete t)



