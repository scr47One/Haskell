
{- numerosRomanos regresa la lista de números romanos -}
numerosRomanos :: [String] -- aqui pongo los numeros romanos
numerosRomanos = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
 
{-valor regresa los valores de los numeros romanos -}
valor :: [Integer] -- aquí pongo los valores de los números romanos y los casos especiales
valor = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
 
{- juntarN es para que sepamos el valor de cada numero romano, asociado a su valor -}
{- recibe una lista de enteros y una lista de strings y regresa una lista de tuplas (Integer, String) -}
{- caso base por si las listas están vacías -}                              
{- esta función junta las dos cabezas de la lista y los inserta en la lista resultante -}      
juntarN :: [Integer]->[String]->[(Integer,String)]-- esta funciaon junta en una lista de tuplas al valor y al numero romano
juntarN [] [] = []
juntarN (x:xs) (s:sc) = (x,s):juntarN xs sc
juntarN [] _= []
juntarN _ [] = []

{- charToS convierte un char a un String -}
charToS :: Char -> String
charToS a = [a]

{- separadorValores separa los valores básicos (I,V,X...),y los valores compuestos o diferentes (IV,CL,CM)-}
{- Esta funcion recibe un String que es igual al número romano que se quiere convertir y a la lista de numeros con su respectivo valor-}
{- Este es el caso base si es que el número que se ingresó está vacío -}
{- x es la cabeza de la lista del número
   xs es el cuerpo de la lista del número
   (v,b) es la cabeza de la lista de valores y numeros romanos, donde: v es el valor  y b es es simbolo o número romano
   bv es el cuerpo de la lista de valores y numeros romanos
   la función compara x con b para saber su valor he inserte en la lista si encuentra el valor
   si no salta al siguiente valor en el cuerpo de la lista juntarN
   al final regresa la lista con el nuero romano y su respectivo valor-}                                                                        
separadorValores :: String -> [(Integer, String)]-> [(Integer, String)] -- aqui entra el numero romano, lo divide poniendole su valor correspondiente
separadorValores "" _ = []
separadorValores (x:xs) ((v,b):bv) = if charToS x == b then (v,b):separadorValores xs (juntarN valor numerosRomanos)
else  separadorValores (x:xs) bv
separadorValores _ [] = []

{- diferentesC regresa la lista de valores que son diferentes de los básicos por ejemplo: IX, IV, CM, CD -}
{- el caso base regresa una lista vacía en caso de que ingrese algo vacío -}
{- en otro caso comparamos los valores de acuerdo a lo siguiente
v es el valor de la cabeza de la lista
b es el numero romano de la cabeza de la lista
n es el valor del segundo dato de la lista
m es el numero romano del segundo dato de la lista
bv es el cuerpo de la lista.
comparamos el valor v con n y en caso de que el primero sea menor que el segundo significa que este es un caso diferente de los básicos
puesto que de acuerdo a su jerarquía y su valor por ejemplo el IV  es cuatro y separados serían 1 y 5 respectivamente
en ese caso guardamos el valor dentro de la lista de diferentesC y omitimos esos números, en otro caso mandamos el segundo valor y 
el cuerpo restante de la lista para no perder los valores cabe mencionar que regresa en las tuplas los dos numeros romanos sin valor
para poder compararlos después-}
diferentesC :: [(Integer,String)] -> [(String,String)]
diferentesC ((v,b):(n,m):bv)= if v>=n then diferentesC ((n,m):bv)
else  (b,m):diferentesC bv
diferentesC [] = []
diferentesC [(_,_)]=[]

{- basicos regresa la suma de los números básicos que se pueden sumar porque siguen la jerarquía que tienen los números romanos
recibe la lista de las tuplas y regresa la suma -}
{- el caso base es que si llega una lista vacía regresa 0
en otro caso hace los siguiente de acuerdo a:
v es el valor de la cabeza de la lista
b es el numero romano de la cabeza de la lista
n es el valor del segundo dato de la lista
m es el numero romano del segundo dato de la lista
bv es el cuerpo de la lista.
comparamos v con n y en caso de que sea menor mandamos sólo el cuerpo de la lista ya que esta vez no nos interesan los casos diferentes
en caso contrario, si es mayor o igual sumamos v, n y regresamos a la función para que siga sumandolos
si no regresa 0
en caso de que ya sólo existan dos valores regresa v y lo suma-}
basicos :: [(Integer,String)] -> Integer
basicos ((v,b):(n,m):bv) = if v>= n then v+basicos ((n,m):bv)
else basicos bv
basicos ((v,b):[])=v
basicos []=0
basicos [(_,_)]=0

{- sumaDif recibe la lista de los numeros, la lista de valores y regresa la suma de estos -}
{- recibimos una lista de tuplas con los numeros romanos de los casos diferentes para compararlos con la lista de valores que tenemos
de acuerdo a lo siguiente:
(v,b) es la tupla de numeros romanos sin valor
(n,m) es la tupla de valor y numero romano
xs y bv son los cuerpos de lista de los numeros romanos sin valor y con valor respectivamente
primero concatenamos v con b y lo comparamos con el numero romano de la lista en caso de que sean iguales
sumamos el valor asociado del numero romano y comparamos al siguiente, en otro caso nos vamos al siguiente valor en la lista a comparar
 -}
sumaDif :: [(String,String)]-> [(Integer, String)]->Integer
sumaDif ((v,b):xs)  ((n,m):bv)= if v++b == m then n+sumaDif xs ((n,m):bv)
else sumaDif ((v,b):xs) bv
sumaDif _ []= 0
sumaDif [] _ = 0


{- sumaTodo suma dos valores y te regresa el resultado -}
sumaTodo:: Integer -> Integer -> Integer
sumaTodo a b = a+b


main :: IO ()
main = do
print "Convertidor de romanos a decimales"
putStr "Digita el número romano (Entre comillas y con mayusculas)->  "
num <-getLine
putStr "el resultado es :"
print(show( sumaTodo (sumaDif (diferentesC(separadorValores(read num)(juntarN (valor) (numerosRomanos)))) (juntarN (valor) (numerosRomanos))) (basicos( separadorValores (read num) (juntarN valor numerosRomanos) ) ) ) )
