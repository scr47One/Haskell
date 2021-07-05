
{- n_tail:: [Int] -> [Int]
n_head :: [Int] -> Int
n_head (x:xs)=x
n_tail (x:xs)=xs

tamano :: [Int] -> Int
tamano[]=0
tamano(x:xs)=1+tamano xs

tamanos :: [Int] -> Int
tamanos[]=0
tamanos(x:xs)=x+tamanos xs

reversa :: [Int] -> [Int]
reversa []=0 -}
{- 
ordenada :: [Int] -> Bool
ordenada
 -}