import Data.Array as Ar
import Data.Function
import Data.List

data Action =  N | A | D | M
instance Show Action where
    show A = "Add"
    show N = "Nothing"
    show D = "Delete"
    show M = "Modify"


cost :: Action -> Int
cost N = 0
cost _ = 1



distance :: [Char] -> [Char] -> (Int , [Action])
distance a b = (u, w)
    where
         (m,n) = (length a, length b)
         bnds = ((0,0), (m,n))

         table = Ar.listArray bnds [d i j | (i,j) <- Ar.range bnds]

         f::Int -> Int ->Action -> (Int, [Action])
         f i j action = (u + cost action, action:v)
              where (u,v) = table Ar.! (i,j)

         d::Int -> Int -> (Int, [Action])
         d 0 0 = (0, [])
         d i 0 = f (i-1) 0 D
         d 0 j = f 0 (j-1) A
         d i j | a Ar.! (i, j) == b Ar.!(i, j) = f (i-1) (j-1) N
               |otherwise = mini [ f i (j-1) A, f (i-1) j D, f (i-1) (j-1) M ]

         mini = minimumBy ( compare `on` fst)

         (u,v) = d m n
         w = reverse v