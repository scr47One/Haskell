
potencia :: Integer -> Integer -> Integer
potencia x y = if y == 0 then 1
else x*potencia (x)(y-1)

division :: Integer -> Integer -> Integer
division 0 0 = 0
division 0 i = 0
division i 0 = 0
division a b = 1+ division (a-b) b

examen :: Integer -> Integer -> Integer -> Integer -> Integer
examen m n i it = if it == 0 then 0
else potencia (division n m) m + examen (m+i) (n+1) (i+2) (it-1)

main :: IO ()
main = do
    putStrLn "PROGRAMA 1"
    putStr "Digita el número M -> "
    num <-getLine
    putStr "Digita el número N -> "
    num2 <-getLine
    putStr "Digita la iteración (I) ->  "
    num3 <-getLine
    putStr "el resultado es : "
    putStrLn (show(examen(read num)(read num2)(read num3)(read num3)))

