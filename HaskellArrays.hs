import Data.Array

{-

The Ix library defines a type class of array indices:

class  (Ord a) => Ix a  where
    range       :: (a,a) -> [a]
    index       :: (a,a) a -> Int
    inRange     :: (a,a) -> a -> Bool

Haskell's monolithic array creation function forms an array from a pair of bounds
and a list of index-value pairs (an association list):

    array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b


Here, for example, is a definition of an array of the squares of numbers from 1 to 100:

    squares  =  array (1,100) [(i, i*i) | i <- [1..100]]

We might generalize this example by parameterizing the bounds and the function to be applied to each index:

    mkArray                 :: (Ix a) => (a -> b) -> (a,a) -> Array a b
    mkArray f bnds          =  array bnds [(i, f i) | i <- range bnds]

    Thus, we could define squares as mkArray (\i -> i * i) (1,100).
-}


mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray f bnds = array bnds [(i, f i ) | i <- range bnds]


fibs :: Int -> Array Int Int
fibs n = a where a = array (0 , n ) ([(0,1), (1, 1)] ++ [(i, a!(i-1) + a!(i-2)) | i <- [2..n]])


waveFront :: Int -> Array (Int, Int)  Int
waveFront n  = a where a = array ((1,1),(n,n)) ([((1, j), 1) | j <- [1..n]]
                                                ++ [((i, 1), 1) | i <-[2..n]]
                                                ++ [((i, j), a!(i-1, j) + a!(i, j-1) + a!(i-1, j-1))
                                                | i <- [2..n], j <- [2..n] ] )


{-
 Example :
 waveFront 4        => array ((1,1),(4,4)) [
                                            ((1,1),1),((1,2),1),((1,3),1),((1,4),1),
                                            ((2,1),1),((2,2),3),((2,3),5),((2,4),7),
                                            ((3,1),1),((3,2),5),((3,3),13),((3,4),25),
                                            ((4,1),1),((4,2),7),((4,3),25),((4,4),63)
                                            ]

-}

{-
accumArray function :

accumArray :: Ix a => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b

example  ::
accumArray (+) 0 (1,4) [(1,10), (1, 5), (2, 3), (3, 1), (4, 4)]                 => array (1,4) [(1,15),(2,3),(3,1),(4,4)]

-}