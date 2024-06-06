import Data.Char 
myLen [] = 0
myLen (h:t) = 1 + myLen t

checkSorted [f,s] = if (f >= s) then True else False
checkSorted (h:(h2:t)) = if (h >= h2) then checkSorted (h2:t) else False 

bubble [a,b] = if (a>=b) then [a,b] else [b,a] 
bubble (h:(h2:t)) = if (h >= h2) then (h: bubble (h2:t)) else (h2 : bubble (h:t))

bSort x = if (checkSorted x) then x else bSort(bubble(x))

zipNsort (h1:t1) (h2:t2) = if (h1 >= h2) then h1: zipNsort t1 (h2:t2) else h2: zipNsort t2 (h1:t1)
zipNsort [] (a)  = (a) 


h1 s = take (div (length s) 2) s
h2 s = drop (div (length s) 2) s

ssc [] [a] = [a]
ssc [b] [a] =  if (b > a) then [b,a] else [a,b]
ssc l1 l2 = zipNsort (ssc (h1 l1) (h2 l1)) (ssc (h1 l2) (h2 l2)) 

sscStart a = (ssc (h1 a) (h2 a))





zipNsortF (h1:t1) (h2:t2) f = if (f h1 h2) then h1: zipNsortF t1 (h2:t2) f else h2: zipNsortF t2 (h1:t1) f
zipNsortF [] (a) f = (a) 

sscF [] [a] f = [a]
sscF [b] [a] f =  if (f b  a) then [b,a] else [a,b]
sscF l1 l2 f = zipNsortF (sscF (h1 l1) (h2 l1) f) (sscF (h1 l2) (h2 l2) f) f

sscStartF a f = (sscF (h1 a) (h2 a) f)

xseq x = [x, x+3 ..]
yseq y = [y, y+5 ..]


cToI :: Char -> Float
cToI x = fromIntegral (ord x - ord '0') 

sToI :: [Char] -> Float 
sToI [] = 0
sToI (h:t) = ((cToI h) * fromIntegral(10 ^ (length t))  + sToI(t)) 

sToIs :: [Char] -> Float 
sToIs [] = 0
sToIs (h:t) = ((cToI h) / 10 )  + ( sToIs(t) / 10.0 ) 

nice x = (drop 1 x)
findDot (h:t) y = if (h == '.') then y else findDot(t) y+1  

formatToNum s = sToI (take (findDot (nice s) 0) (nice s)) + sToIs (drop ((findDot (nice s) 0) + 1) (nice s))


greatestDenominator x y =  last [a | a <- [1 .. (min x y) -1] , ((mod y a) == 0), ((mod x a) == 0)]

isPrime x =  (( last [a | a <- [1 .. x-1] , ((mod x a) == 0)] ) == 1)


valid a b =  (a == ' ') && (b == 5)
lbreak [] c = []
lbreak (h:t) c = if (valid h c) then ('\n': (h:lbreak t 0))  else  (h:lbreak t (c+1))

