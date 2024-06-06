--stagger :: (Num t) => t  -> t
--stagger d = d + 2
--crawl d = d + 1
--
--treasureMap d = crawl ( stagger ( stagger d))
--
----still uggly but I guess it looks procedural
--letTreasureMap (d) = let d1 = stagger d; d2 = stagger d1; d3 = crawl d2;in d3

data Position t = Position t deriving (Show)
--what does this do? defines Position data type. which is always marked with position num

--these are just normal ass functions 
--the syntax is actually pretty nice, in a way it feels like we are doing a pattern match and a new pattern
stagger (Position d) = Position (d+2)
crawl (Position d) = Position (d+1)

rtn x = x
x >>== f = f x


ryIo = do putStr "Enter your name: "; line <- getLine ; let {backwards = reverse line} ; return backwards

crack = do x <- ['a'..'c'] ; y <- ['a'..'c'] ; z <- ['a' .. 'c']; let {password = [x,y,z]} ; if attempt password then return (password, True) else return (password, False)

attempt pw = if pw == "cab" then True else False

data Maybe a = N | J a 
instance Monad Maybe where 
     return = J  
     N >>= f = N  
     (J x) >>= f = f x
