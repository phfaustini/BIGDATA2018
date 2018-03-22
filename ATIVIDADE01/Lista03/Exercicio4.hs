module Main where
    
fibonacci = 0 : 1 : fib fibonacci
    where fib (x:xs) = (x + head xs) : fib xs


{-Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares 
dos valores que não excedem 4.000.000. (Project Euler 2)-}
exercicio4 = sum $ filter even (takeWhile (<=4000000) fibonacci)

main :: IO ()
main = print exercicio4
    