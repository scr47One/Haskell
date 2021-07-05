

unirListas ::[Integer] -> [Integer] ->[Integer]
unirListas [] [] = []
unirListas [] p = p
unirListas p [] = p
unirListas xs (d:dc) = d:unirListas xs dc

quitarRep :: [Integer] -> [Integer]
quitarRep [] = []
quitarRep (x:xs) = x : quitarRep (filter (/= x) xs)

numerosRomanos :: [String] -- aqui pongo los numeros romanos
numerosRomanos = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
 
valor :: [Integer] -- aquí pongo los valores de los números romanos y los casos especiales
valor = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
 
juntarN :: [Integer]->[String]->[(Integer,String)]-- esta funciaon junta en una lista de tuplas al valor y al numero romano
juntarN [] [] = []
juntarN (x:xs) (s:sc) = (x,s):juntarN xs sc
juntarN [] _= []
juntarN _ [] = []

charToS :: Char -> String
charToS a = [a]

separadorValores :: String -> [(Integer, String)]-> [(Integer, String)] -- aqui entra el numero romano, lo divide poniendole su valor correspondiente
separadorValores "" _ = []
separadorValores (x:xs) ((v,b):bv) = if charToS x == b then (v,b):separadorValores xs (juntarN valor numerosRomanos)
else  separadorValores (x:xs) bv
separadorValores _ [] = []


diferentesC :: [(Integer,String)] -> [(String,String)]
diferentesC ((v,b):(n,m):bv)= if v>=n then diferentesC ((n,m):bv)
else  (b,m):diferentesC bv
diferentesC [] = []
diferentesC [(_,_)]=[]

basicos :: [(Integer,String)] -> Integer
basicos ((v,b):(n,m):bv) = if v>= n then v+basicos ((n,m):bv)
else basicos bv
basicos ((v,b):[])=v
basicos []=0
basicos [(_,_)]=0

sumaDif :: [(String,String)]-> [(Integer, String)]->Integer
sumaDif ((v,b):xs)  ((n,m):bv)= if v++b == m then n+sumaDif xs ((n,m):bv)
else sumaDif ((v,b):xs) bv
sumaDif _ []= 0
sumaDif [] _ = 0


sumaTodo:: Integer -> Integer -> Integer
sumaTodo a b = a+b


main :: IO ()
main = do
print "Convertidor de romanos a decimales"
putStr "Digita el número romano (Entre comillas y con mayusculas)->  "
num <-getLine
putStr "el resultado es :"
print(show( sumaTodo (sumaDif (diferentesC(separadorValores(read num)(juntarN (valor) (numerosRomanos)))) (juntarN (valor) (numerosRomanos))) (basicos( separadorValores (read num) (juntarN valor numerosRomanos) ) ) ) )
print "Programa que une dos listas y quita repetidos"
putStr "Ingrese primer lista -> "
list <-getLine
putStr "Ingrese segunda lista ->"
list2 <-getLine
print(show( quitarRep (unirListas  (read list) (read list2)) ) )
