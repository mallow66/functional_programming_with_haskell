import qualified Data.Map as M
import qualified HeapStructure as T

type Code = [Char]
data HT = Leaf Char Int | Node Int HT HT deriving (Show)

instance Eq HT where
    (==) h1 h2 = frq h1 == frq h2

instance Ord HT where
    compare h1 h2 = compare (frq h1) (frq h2)


frq :: (HT) ->Int
frq (Leaf _ r) = r
frq (Node r _ _ ) = r


-- Now we gonna encode a text using Huffman tree and the Heap data structure

frequency :: String -> [(Char, Int )]
frequency xs =  M.toList $ M.fromListWith(+) $ map (\x->(x,1)) xs

--transform the tuples into leaves
toLeaf :: (Char, Int) -> ( HT )
toLeaf (c, i) = (Leaf c i)

--insert the leaves in a heap ( build a heap with the leaves nodes)
heap :: String -> T.LH HT
heap xs = T.fromList $ map (toLeaf) (frequency xs)

--construc the huffman tree
construct :: T.LH HT -> HT
construct p | T.size p == 1 = T.minimum' p
            | otherwise = construct $ T.add (Node ((frq m1) + (frq m2)) m1 m2 ) p'
                where (m1, m2, p') = T.getTwoMin (p)

heapTree :: String -> HT
heapTree xs = construct $ heap xs


encode :: HT -> [(Char, Code)]
encode (Leaf c _) = [(c, "")]
encode (Node _ l r ) = map (accumulate '0') (encode l) ++ map (accumulate '1') (encode r)
                        where accumulate c = \(x,y) -> (x,c:y)
-- here we sstock all the codes in a Map structure
mapCode :: String -> M.Map Char Code
mapCode xs =   M.fromList $ encode $ heapTree xs




fromTextToCode :: String -> Code
fromTextToCode str = concat $ map (m M.!) str
            where m = mapCode str


decode :: HT -> Code -> HT -> String
decode (Leaf c _ ) []  t  = [c]
--decode _ [] _ = ""
decode (Leaf c _ ) (xs) t  = c : (decode t xs t)
decode (Node _ l r) (x:xs) t | x =='0'   = decode l xs t
                             | otherwise = decode r xs t



fromCodeToText :: HT ->  Code -> String
fromCodeToText heap xs = decode heap xs heap

-- Here is a simple test
-- let heap = heapTree "mallow66"
-- let key = fromTextToCode "mallow66"     ==> "11111001011001010000"
-- fromCodeToText heap key                 ==> "mallow66"