
facta :: Integer -> Integer
facta n = if n == 0 then 1
else n * facta (n-1)

sumafac :: Integer -> Integer -> Integer
sumafac n i = if i == 0 then 0
else if i<=n then ((facta n)*n) + sumafac (n-1) (i-1)
else 0

sumatoria :: Double -> Double
sumatoria 0 = 0
sumatoria n = n*(n+1)/2 + sumatoria (n-1)

main :: IO ()
main = do
    putStrLn "PROGRAMA 1"
    putStr "Digita el número -> "
    num <-getLine
    putStr "Digita el número de iteración -> "
    num2 <-getLine
    putStr "el resultado es : "
    putStrLn (show(sumafac(read num)(read num2)))
    putStrLn "PROGRAMA 2 sumatoria n hasta 1 + n-1 hasta 1 + n-2 hasta 1"
    putStr "Digita el número -> "
    num3 <-getLine
    putStrLn (show(sumatoria(read num3)))