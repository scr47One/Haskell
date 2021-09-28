
pot :: Integer -> Integer -> Integer
pot x 0 = 1
pot x y = x*pot x (y-1)

serie2:: Integer -> Integer -> Integer -> Integer
serie2 n m iter = if iter == 0 then 0
else pot n m + serie2 (n*2) (m+1) (iter-1)

-- >>> suma 4 2 2 3
-- 110076362816
--

suma :: Integer -> Integer -> Integer -> Integer -> Integer
suma m x n 0 = 0
suma m x n i= (pot (m*x) n)+(suma m (x+2) (n+3) (i-1))

main=do 
    putStrLn"Ingresa la Base"
    m<-getLine
    putStrLn"Ingresa la Potencia"
    p<-getLine
    putStrLn"Ingresa la Iteración"
    i<-getLine
    putStrLn"El resultado es="
    putStrLn(show(suma (read m) 2 (read n) (read i)))