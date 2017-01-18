data Color = BLACK | RED
data RBT a = E | N a Color (RBT a) (RBT a) deriving (Show)