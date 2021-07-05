potencia :: Integer -> Integer -> Integer
potencia x y | y == 0 = 1
            | otherwise = x*potencia (x)(y-1)

division :: Integer -> Integer -> Integer
division 0 i = 0
division a b = 1+ division (a-b) b

examen :: Integer -> Integer -> Integer -> Integer -> Integer
examen m n i it | it == 0 = 0
        | otherwise = potencia (division n m) m + examen (m+i) (n+1) (i+2) (it-1)

main :: IO ()
main = do

    putStr "Digita el número M -> "
    num <-getLine
    putStr "Digita la potencia o número n -> "
    num2 <-getLine
    putStr "Digita la iteración ->  "
    num3 <-getLine
    putStr "el resultado es : "
    putStrLn (show(examen(read num)(read num2)(read num3)(read num3)))