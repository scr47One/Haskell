main :: IO()
main = do
 putStr "Introduce n =>"
 n<-getLine
 putStr "Introduce m =>"
 m<-getLine
 putStr "Introduce que número de Iteraciones=>"
 i<-getLine
 putStrLn(show(ejercicio (read n) (read m) (read i)))

ejercicio :: Integer -> Integer -> Integer -> Integer
ejercicio n m 0 = 0
ejercicio n m i = (pot(divi(n)(m))(m)) + ejercicio(n+1)(m+2)(i-1)

divi :: Integer -> Integer -> Integer
divi n m = if n<m then 0
else 1+ divi(n-m)(m)

pot :: Integer->Integer->Integer
pot x 0=1
pot x y = x*pot(x)(y-1)