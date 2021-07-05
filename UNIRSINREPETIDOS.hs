{- Unir unirListas recibe dos listas y las une
    En esta parte recibo dos listas de enteros -}
{- El primer caso base es por si las dos listas se acaban me regrese un a lista vacía -}
{- El segundo caso base es por si la primera lista es vacía me regrese el resto de la segunda lista -}
{- El tercer caso base es por si la segunda lista es vacía me regrese el resto de la primera lista -}
{- xs es la pirmer lista, d es la cabeza de la segunda lista, dc es el cuerpo de la segunda lista,
 la funcion pone d a la cabeza de unirListas hasta que termine en alguno de los casos base-}
unirListas ::[Integer] -> [Integer] ->[Integer]    
unirListas [] [] = []                               
unirListas [] p = p                                 
unirListas p [] = p                                 
unirListas xs (d:dc) = d:unirListas xs dc

{- quitarRep recibe dos listas de enteros y te regresa -}
{- el caso base regresa una lista vacía si recibe una lista vacía -}
{- en este caso filtramos la lista con la función filter donde cada elemento de xs sea diferente de x -}
quitarRep :: [Integer] -> [Integer]                 
quitarRep [] = []                                   
quitarRep (x:xs) = x : quitarRep (filter (/= x) xs)

main :: IO ()
main = do
print "Programa que une dos listas y quita repetidos"
putStr "Ingrese primer lista -> "
list <-getLine
putStr "Ingrese segunda lista ->"
list2 <-getLine
print(show( quitarRep (unirListas  (read list) (read list2)) ) )
