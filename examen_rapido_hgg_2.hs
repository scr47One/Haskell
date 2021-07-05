
potencia :: Integer -> Integer -> Integer
potencia x y | y == 0 = 1
            | otherwise = x*potencia (x)(y-1)

examen :: Integer -> Integer -> Integer -> Integer -> Integer
examen m n i it | it == 0 = 0
        | otherwise = potencia (m+i) n + examen m (n+1) (i+1) (it-1)
-- >>> examen 3 2 3 3
-- 4475
--

main :: IO ()
main = do
    putStrLn "PROGRAMA 1"
    putStr "Digita el número M -> "
    num <-getLine
    putStr "Digita la potencia o número n -> "
    num2 <-getLine
    putStr "Digita la iteración ->  "
    num3 <-getLine
    putStr "el resultado es : "
    putStrLn (show(examen(read num)(read num2)(read num3)(read num3)))


