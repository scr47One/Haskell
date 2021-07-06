facta :: Integer -> Integer
facta n = if n == 0 then 1
else n * facta (n-1)

sumafac :: Integer -> Integer -> Integer
sumafac n i = if i == 0 then 0
else ((facta n)*n) + sumafac (n-1) (i-1)


-- >>> sumafac 0 1
-- 0
--
