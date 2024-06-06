data Suit = Spades | Hearts deriving (Show)
data Rank = Ten | Jack | Queen | King | Ace deriving (Show)
type Card = (Rank, Suit)
type Hand= [Card]

value :: Rank -> Integer
value Ten = 1
value Jack = 2
value Queen = 3
value King = 4 
value Ace = 5

cardVal :: Card -> Integer
cardVal (rank,suit) = value rank


backwards :: [a] -> [a]
backwards [] = []
backwards (h:t) = backwards t ++ [h]


data Triplet a = Trio a a a deriving (Show)
data Tree a = Children [Tree a] | Leaf a deriving (Show)

depth (Leaf _) = 1
depth (Children c) = 1 + maximum (map depth c)

class Eq a where 
     (==), (/=) :: a -> a -> Bool
     x (/=) y = not (== x y)
     x (==) y = not (\= x y)


