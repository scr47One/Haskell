facta :: Integer -> Integer
facta n = if n == 0 then 1
else n * facta (n-1)

sumafac :: Integer -> Integer -> Integer
sumafac n i = if i == 0 then 0
else ((facta n)*n) + sumafac (n-1) (i-1)

llenaLista :: Integer -> Integer -> [Integer]
llenaLista  0 m = []
llenaLista  x m = m:llenaLista (x-1) m

-- >>> llenaLista 2 3
-- [3,3]
--
