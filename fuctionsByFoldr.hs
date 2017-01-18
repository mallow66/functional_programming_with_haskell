 --                " Here is some examples of some functions written by Forldr "

 -- sum of list elements

 sum'::(Num a) => [a] -> a
 sum' [] = 0
 sum' (x:xs) = x + sum' xs


 sum'':: (Num a) =>[a] -> a
 sum'' xs = foldr(+) 0 xs


 --multiplication of list elements

 mul' :: (Num a) => [a] -> a
 mul' [] = 1
 mul' (x:xs) = x * mul' xs

 mul'':: (Num a) =>[a] -> a
 mul'' xs = foldr(*) 1 xs



-- concat function
 concat' [] = ""
 concat' (x:xs) = [x] ++ concat' xs


 concat'' xs = foldr(++) "" xs


-- apply a function for every element of the list

 map' f [] = []
 map' f (x:xs) = f x : map f xs


 map'' f xs = foldr( \ x ys -> f x : ys ) [] xs


 --filter a list based on a predicate p

 filter' :: (a -> Bool ) -> [a] -> [a]
 filter' p (x:xss) | p x = x: filter' p xs
                  | otherwise =  filter' p xs


 filter'' p xs = foldr (f) [] xs
                where f x ys = if (p x ) then x:ys else ys


   -- sometimes we need to construct the the sequence of the elements starting with the last element of the List
      -- in this case we can use foldl
      -- the definition of this function is
      -- foldl :: (b -> a -> b ) -> b -> [a] -> b
      -- foldl f e [] = e
      -- foldl f e (x:xs) = foldl f (f e x) xs

      -- for example we can use it in the reverse function that reverse the elements of a list.



 reverse' :: [a] -> [a]
 reverse' [] = []
 reverse' ( x : xs ) = reverse' xs ++ [x]

 reverse'' :: [a] ->[a]
 reverse'' xs = foldl(flip(:)) [] xs








