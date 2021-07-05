
pot :: Integer -> Integer -> Integer
pot x 0 = 1
pot x y = x*pot x (y-1)

serie2:: Integer -> Integer -> Integer -> Integer
serie2 n m iter = if iter == 0 then 0
else pot n m + serie2 (n*2) (m+1) (iter-1)

-- >>> serie2 2 3 2
-- 264
--

