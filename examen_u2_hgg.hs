
potencia :: Integer -> Integer -> Integer
potencia x y = if y == 0 then 1
else x*potencia (x)(y-1)

division :: Integer -> Integer -> Integer
division a b | b >= a = 0
    | a== 0 = 0
    | b==0 = 0
    | otherwise = 1+ division (a-b) b

examen :: Integer -> Integer -> Integer -> Integer
examen m n it = if it == 0 then 0
else potencia (division n m) m + examen (m+2) (n+1) (it-1)

main :: IO ()
main = do
    putStrLn "Examen"
    putStr "Digita el número M -> "
    num <-getLine
    putStr "Digita el número N -> "
    num2 <-getLine
    putStr "Digita la iteración (I) ->  "
    num3 <-getLine
    putStr "el resultado es : "
    putStrLn (show(examen(read num)(read num2)(read num3)))

