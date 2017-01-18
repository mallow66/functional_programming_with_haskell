data Color = BLACK | RED
data RBT a = E | N a Color (RBT a) (RBT a) deriving (Show)

toBlack :: (RBT a) -> (RBT a)
toBlack E = error"Empty Node"
toBlack (N x c l r)  = (N x BLACK l r)