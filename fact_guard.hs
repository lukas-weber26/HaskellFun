factorial:: Integer -> Integer 
factorial x
     | x>1 = x*factorial(x-1)
     | otherwise = 1

fibTouple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTouple (x,y,0) = (x,y,0)
fibTouple (x,y, index) = fibTouple (y,x+y,index-1)

fibResult :: (Integer, Integer, Integer) -> Integer
fibResult(x,y,z) = x

fib :: Integer -> Integer
fib x = fibResult(fibTouple(0,1,x))

fibNextPair :: (Integer, Integer) -> (Integer, Integer)
fibNextPair (x,y) = (y,x+y)

fibNthPair :: Integer -> (Integer, Integer)
fibNthPair 1 = (1,1)
fibNthPair n = fibNextPair(fibNthPair(n-1))


size [] = 0
size(h:t) = 1 + size t

prod[] = 1
prod(h:t) = h*prod(t)

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (h:t) = if even h then h:allEven t else allEven t

allEven2 :: [Integer] -> [Integer]
allEven2 [] = []
allEven2 (h:t) = if ((mod h 2) == 0) then h:allEven2 t else allEven2 t

rev :: [Integer] -> [Integer]
rev [] = []
rev (h:t) =  rev(t) ++ [h]


