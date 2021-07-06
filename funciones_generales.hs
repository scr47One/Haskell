main1 :: IO()

contador :: Integer -> Integer -> String
contador x y = if x == y then show y
else contador (x+1) y

division :: Integer -> Integer -> Integer
division a b | b > a = 0
    | a== 0 = 0
    | b==0 = 0
    | b==a= 1+ division (a-b) b
    | otherwise = 1+ division (a-b) b
-- >>> division 16 4
-- 2

cont :: Integer -> Integer -> Integer
cont num 0=0
cont num x =num + cont num (x-1)

serie :: Integer -> Integer -> String
serie num 0 = ""
serie num x = show(cont num x)++""++(serie num (x-1))

main1=do
    putStr "Introduce hasta donde llegará el contador =>"
    x<-getLine
    putStr "Introduce que número contará"
    num<-getLine
    putStrLn(show(serie (read num) (read x)))

sumap :: Integer -> Integer-> Integer
sumap x 1 = pot x 1
sumap x y = pot x y + sumap x (y-1)

pot :: Integer -> Integer -> Integer
pot x 0 = 1
pot x y = x*pot x (y-1)

potencia :: Int -> Int -> Int
potencia m 0 =0
potencia m n = (m*(potencia m (n-1))+m)

{- ((N+1)! ^(M!)), ((N+2)!^(M!)), ((N+3)!^(M!)) -}
facta :: Integer -> Integer
facta n = if n == 0 then 1
else multiplicar n  (facta (n-1))

multiplicar :: Integer -> Integer -> Integer
multiplicar x 0 = 0
multiplicar x y =  x+multiplicar x (y-1)

ejercicio :: Integer -> Integer -> Integer -> Integer
ejercicio n m c = if c == 0 then 0
else (pot (facta (n+1)) (facta m)) + ejercicio (n+1) m (c-1)

-- >>> ejercicio 9 9 3
-- ProgressCancelledException


contador1  :: Integer -> Integer -> Integer
contador1 num 0 =0
contador1 num iter = ((num*iter)+(iter*2))+(contador1 num (iter-1))

ejecutar = do
putStr "Digita el número"
num <-getLine
putStr "el resultado es :"
putStrLn (show(contador1(read num)(read num)))

{- contador2 :: Integer-> Integer-> Integer
contador2 m z = if z <= m then ((z*m)+(z+1))+contador2 m (z+1)
else 0
 -}
{- serie :: Integer -> Integer -> Integer
serie 0 n = 0
serie i n =  -}
