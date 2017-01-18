-- Sort by Insertion

--insert an element in a sorted list and keeping it sorted
insert :: (Ord a) => a->[a]->[a]
insert el [] = [el]
insert el (x:xs) | el<=x = el:(x:xs)
                 | otherwise = x:(insert el xs)

insertionSort::(Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)


--Quick Sort

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort [x] = [x]
qSort (x:xs) = (qSort inf) ++ [x] ++ (qSort sup)
               where
                    inf = [ y | y <- xs, y <= x ]
                    sup = [ y | y <- xs, y > x ]



--Sort by selection

--We gonna need a Minimum function to calculate the minimum of a list
minimum' :: (Ord a) => [a] -> a
minimum' [] = error"Empty List"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- we gonna need a function delete' to delete the list's minimum
delete':: (Eq a) => a->[a]->[a]
delete' _ [] = []
delete' el (x:xs) | el == x = xs
                  |otherwise = x:delete' el xs

selectionSort :: (Ord a) => [a]->[a]
selectionSort [] = []
selectionSort xs = y : selectionSort xs'
                    where y = minimum xs
                          xs' = delete' y xs


-- Sort by Fusion

--we will use a function 'halve' to split a list into two equal lists
halve :: [a] -> ([a], [a])
halve xs = splitAt ((length xs)`div`2) xs

-- we will use a function 'fusion' to merge two sorted lists
fusion :: (Ord a) => [a] -> [a] -> [a]
fusion [] ys = ys
fusion xs [] = xs
fusion l1@(x:xs) l2@(y:ys) | x<=y = x: (fusion xs l2)
                           | otherwise = y:(fusion l1 ys)


fusionSort :: (Ord a) => [a] ->[a]
fusionSort [] = []
fusionSort [x] = [x]
fusionSort xs = fusion (fusionSort l) (fusionSort r)
                where (l,r) = halve xs