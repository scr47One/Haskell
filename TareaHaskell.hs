tablas1:: Integer->Integer->Integer->Integer->Integer->String
tablas1 a n b 0 d=tabla(a+1)(n-b)(b)(n-1)(d-1)
tablas1 a n b x d=show(a*n)++" "++tablas1(a)(n+1)(b)(x-1)(d)

tabla:: Integer->Integer->Integer->Integer->Integer->String
tabla a n b x 0=" "
tabla a n b x d= (tablas1 (a)(n)(b)(x)(d))

main = do
putStr "Introduce la tabla a multiplicar => "
a<-getLine
putStr "Introduce el inicio de las tablas => "
n<-getLine
putStr "Introduce el final de las tablas => "
b<-getLine
putStr "Introduce hasta que tabla deseas => "
d<-getLine
putStrLn(tabla(read a)(read n)(read b)((read b)-(read n)+1)(read d))

