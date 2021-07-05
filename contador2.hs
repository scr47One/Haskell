contador::Integer-> Integer-> Integer

contador num 0=0
contador num iter= ((num*iter)+(iter*2))+(contador num (iter-1))

main::IO()

main=do
putStr "Digita el número N =>"
num <- getLine
putStr "El resultado es: "
putStrLn (show(contador(read num) (read num)))

